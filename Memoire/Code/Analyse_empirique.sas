/* ============================================================
   Multi-pays : US + France + Allemagne + UK
   Modeles : Probit Simple/Enrichi + HPLOGIT Simple/Enrichi
   Split : 70% IS / 30% OOS
   ============================================================ */

%let path = C:\Users\oscar\Desktop\Memoire;

ods graphics on / reset=all width=26cm height=17cm imagefmt=png;


/* ============================================================
   DECLARATIONS GLOBALES des macro-variables HP
   (on en a besoin car run_hpmodel est imbriquee dans analyse_pays)
   ============================================================ */
%global tp_hp_is tn_hp_is fp_hp_is fn_hp_is
        tp_hp_oo tn_hp_oo fp_hp_oo fn_hp_oo
        auc_hp_is r2_hp_is auc_hp_oos;


/* ============================================================
   INITIALISATION DES DATASETS PANNEAUX
   ============================================================ */

data synthese;
   length pays $15 modele $30 scope $5;
   pays=''; modele=''; scope='';
   TP=.; TN=.; FP=.; FN=.; auc=.; pseudo_r2=.;
   delete;
run;

%macro init_panel(nom=, vars=);
   data &nom;
      length pays $15;
      pays=''; date_dec=.; &vars; rec_lo=.; rec_hi=.; delete;
   run;
%mend;

%init_panel(nom=panel_spread,           vars=%str(spread=.; lisse=.; recession=.));
%init_panel(nom=panel_probit_simple,    vars=%str(prob=.; recession=.));
%init_panel(nom=panel_probit_enrich,    vars=%str(prob=.; recession=.));
%init_panel(nom=panel_oos_simple,       vars=%str(prob=.; recession=.));
%init_panel(nom=panel_oos_enrich,       vars=%str(prob=.; recession=.));
%init_panel(nom=panel_logit_simple,     vars=%str(prob=.; recession=.));
%init_panel(nom=panel_logit_enrich,     vars=%str(prob=.; recession=.));
%init_panel(nom=panel_oos_logit_simple, vars=%str(prob=.; recession=.));
%init_panel(nom=panel_oos_logit_enrich, vars=%str(prob=.; recession=.));

%global _r2g_;


/* ============================================================
   MACRO PRINCIPALE PAR PAYS
   ============================================================ */

%macro analyse_pays(pays=, an_debut=1992, an_fin=2023);

   %put ===== &pays =====;

   proc import datafile="&path\Donnees_&pays..csv"
       out=_don_ dbms=csv replace;
       getnames=yes;
   run;

   data mk_&pays;
      set _don_;
      length pays $15;
      t + 1;
      date_dec  = annee + (trim - 1) / 4;
      spread_l4 = lag4(spread_brut);
      petro_l4  = lag4(var_petrole);
      cli_l4    = lag4(var_cli);
      rate_l4   = lag4(rate_dir);
      etf_l4    = lag4(var_etf);
      if t > 4
         and spread_l4 ne . and petro_l4 ne .
         and cli_l4    ne . and rate_l4   ne .
         and etf_l4    ne .
         and annee >= &an_debut and annee <= &an_fin;
      pays = "&pays";
   run;

   %put mk_&pays : %sysfunc(attrn(%sysfunc(open(mk_&pays)),nobs)) obs;

   proc sql noprint;
      select min(spread_brut) - 0.3, max(spread_brut) + 0.3
      into :smin trimmed, :smax trimmed
      from _don_
      where spread_brut ne .
        and annee >= &an_debut and annee <= &an_fin;
   quit;

   data _ps_;
      length pays $15;
      set _don_;
      where spread_brut ne . and spread_kalman ne .
            and annee >= &an_debut and annee <= &an_fin;
      date_dec = annee + (trim - 1) / 4;
      spread   = spread_brut;
      lisse    = spread_kalman;
      pays     = "&pays";
      if recession = 1 then do; rec_lo = &smin; rec_hi = &smax; end;
      else do; rec_lo = .; rec_hi = .; end;
      keep pays date_dec spread lisse recession rec_lo rec_hi;
   run;
   proc append base=panel_spread data=_ps_ force; run;

   proc sql noprint;
      select floor(0.70 * count(*)) into :n_train trimmed
      from mk_&pays;
   quit;

   data train_&pays oos_&pays;
      set mk_&pays;
      if t <= &n_train then output train_&pays;
      else                  output oos_&pays;
   run;

   %put Split -> Train: &n_train obs | OOS: %sysfunc(attrn(%sysfunc(open(oos_&pays)),nobs)) obs;


   /* ----------------------------------------------------------
      MACRO INTERNE PROBIT
   ---------------------------------------------------------- */

   %macro run_model(tag=, vars=, link=probit,
                    panel_is=, panel_oos=,
                    data_is=, data_train=, data_oos=);

      %let _r2g_ = .;

      ods output FitStatistics=_fit_ Association=_assoc_;
      proc logistic data=&data_is descending;
         model recession = &vars / link=&link lackfit ctable pprob=0.5;
         output out=_p_is_ pred=prob;
         title "&tag IS -- &pays";
      run;
      ods output close;

      data _null_;
         set _fit_;
         if strip(Criterion) = '-2 Log L' then
            call symputx('_r2g_',
               put(1 - InterceptAndCovariates / InterceptOnly, best16.), 'G');
      run;
      %let r2_is = &_r2g_;

      proc sql noprint;
         select nValue2 into :auc_is trimmed
         from _assoc_ where Label2 = 'c';
      quit;

      data _p_is_; set _p_is_; pred_bin = (prob >= 0.5); run;
      proc sql noprint;
         select sum(pred_bin=1 and recession=1),
                sum(pred_bin=0 and recession=0),
                sum(pred_bin=1 and recession=0),
                sum(pred_bin=0 and recession=1)
         into :tp_is trimmed, :tn_is trimmed,
              :fp_is trimmed, :fn_is trimmed
         from _p_is_ where pred_bin ne . and recession ne .;
      quit;

      data _gi_;
         length pays $15;
         merge &data_is (keep=t date_dec recession)
               _p_is_  (keep=t prob);
         by t;
         pays = "&pays";
         if recession = 1 then do; rec_lo = 0; rec_hi = 1; end;
         else do; rec_lo = .; rec_hi = .; end;
         keep pays date_dec prob recession rec_lo rec_hi;
      run;
      proc append base=&panel_is data=_gi_ force; run;

      proc logistic data=&data_train descending;
         model recession = &vars / link=&link;
         store out=work._st_;
      run;

      proc plm restore=work._st_;
         score data=&data_oos out=_sc_ / ilink;
      run;

      data _p_oos_;
         length pays $15;
         merge &data_oos (keep=t date_dec recession)
               _sc_      (keep=t Predicted rename=(Predicted=prob));
         by t;
         pays = "&pays";
         pred_bin = (prob >= 0.5);
         if recession = 1 then do; rec_lo = 0; rec_hi = 1; end;
         else do; rec_lo = .; rec_hi = .; end;
      run;

      proc sql noprint;
         select sum(pred_bin=1 and recession=1),
                sum(pred_bin=0 and recession=0),
                sum(pred_bin=1 and recession=0),
                sum(pred_bin=0 and recession=1)
         into :tp_oo trimmed, :tn_oo trimmed,
              :fp_oo trimmed, :fn_oo trimmed
         from _p_oos_ where pred_bin ne . and recession ne .;
      quit;

      ods output Association=_ao_;
      proc logistic data=_p_oos_ descending;
         model recession = prob / link=&link;
      run;
      ods output close;
      proc sql noprint;
         select nValue2 into :auc_oos trimmed
         from _ao_ where Label2 = 'c';
      quit;

      proc append base=&panel_oos
         data=_p_oos_(keep=pays date_dec prob recession rec_lo rec_hi) force;
      run;

      data _row_;
         length pays $15 modele $30 scope $5;
         pays = "&pays"; modele = "&tag"; scope = "IS";
         TP = &tp_is; TN = &tn_is; FP = &fp_is; FN = &fn_is;
         auc = &auc_is; pseudo_r2 = &r2_is; output;
         pays = "&pays"; modele = "&tag"; scope = "OOS";
         TP = &tp_oo; TN = &tn_oo; FP = &fp_oo; FN = &fn_oo;
         auc = &auc_oos; pseudo_r2 = .; output;
      run;
      proc append base=synthese data=_row_ force; run;

      proc datasets library=work nolist;
         delete _fit_ _assoc_ _ao_ _p_is_ _p_oos_ _sc_ _gi_ _row_;
      quit;

   %mend run_model;


   /* ----------------------------------------------------------
      MACRO INTERNE HPLOGIT (ridge Newton-Raphson)
   ---------------------------------------------------------- */

   %macro run_hpmodel(tag=, vars=,
                      panel_is=, panel_oos=,
                      data_is=, data_train=, data_oos=);

      %let tp_hp_is = 0; %let tn_hp_is = 0;
      %let fp_hp_is = 0; %let fn_hp_is = 0;
      %let r2_hp_is = .; %let auc_hp_is = .;
      %let tp_hp_oo = 0; %let tn_hp_oo = 0;
      %let fp_hp_oo = 0; %let fn_hp_oo = 0;
      %let auc_hp_oos = .;

      /* ---- IS ---- */
      ods output FitStatistics=_hp_fit_
                 Association=_hp_assoc_
                 GlobalTests=_hp_glob_;

      proc hplogistic data=&data_is;
         model recession(event='1') = &vars
               / link=logit rsquare association;
         id t;
         output out=_hp_is_ predicted=prob;
         title "HPLOGIT &tag IS -- &pays";
      run;
      ods output close;

      /* R2 Cox-Snell (mais il ne marche pas, probleme à régler) */
      %let r2_hp_is = .;
      proc sql noprint;
         select Value into :r2_hp_is trimmed
         from _hp_fit_
         where upcase(strip(Descr)) = 'R-SQUARE';
      quit;
      %if %quote(&r2_hp_is) = %then %let r2_hp_is = .;

      /* AUC */
      %let auc_hp_is = .;
      proc sql noprint;
         select C into :auc_hp_is trimmed
         from _hp_assoc_;
      quit;
      %if %quote(&auc_hp_is) = %then %let auc_hp_is = .;

      /* Classification IS */
      data _hp_is2_;
         merge &data_is (keep=t recession)
               _hp_is_  (keep=t prob);
         by t;
         pred_bin = (prob >= 0.5);
      run;
      proc sql noprint;
         select sum(pred_bin=1 and recession=1),
                sum(pred_bin=0 and recession=0),
                sum(pred_bin=1 and recession=0),
                sum(pred_bin=0 and recession=1)
         into :tp_hp_is trimmed, :tn_hp_is trimmed,
              :fp_hp_is trimmed, :fn_hp_is trimmed
         from _hp_is2_ where pred_bin ne . and recession ne .;
      quit;

      %let tp_hp_is = &tp_hp_is;
      %let tn_hp_is = &tn_hp_is;
      %let fp_hp_is = &fp_hp_is;
      %let fn_hp_is = &fn_hp_is;

      /* Panel IS */
      data _gihp_;
         length pays $15;
         merge &data_is (keep=t date_dec recession)
               _hp_is_  (keep=t prob);
         by t;
         pays = "&pays";
         if recession = 1 then do; rec_lo = 0; rec_hi = 1; end;
         else do; rec_lo = .; rec_hi = .; end;
         keep pays date_dec prob recession rec_lo rec_hi;
      run;
      proc append base=&panel_is data=_gihp_ force; run;

      /* ---- OOS ---- */
      proc hplogistic data=&data_train;
         model recession(event='1') = &vars / link=logit;
         code file="&path\Code\_hplogit_score_.sas";
         title "HPLOGIT &tag OOS train -- &pays";
      run;

      data _hp_oos_raw_;
         set &data_oos;
         %include "&path\Code\_hplogit_score_.sas";
         /* le code genere P_recession1 = P(event=1) */
         prob = P_recession1;
      run;

      data _hp_oos_;
         length pays $15;
         set _hp_oos_raw_(keep=t date_dec recession prob);
         pays = "&pays";
         pred_bin = (prob >= 0.5);
         if recession = 1 then do; rec_lo = 0; rec_hi = 1; end;
         else do; rec_lo = .; rec_hi = .; end;
      run;

      proc sql noprint;
         select sum(pred_bin=1 and recession=1),
                sum(pred_bin=0 and recession=0),
                sum(pred_bin=1 and recession=0),
                sum(pred_bin=0 and recession=1)
         into :tp_hp_oo trimmed, :tn_hp_oo trimmed,
              :fp_hp_oo trimmed, :fn_hp_oo trimmed
         from _hp_oos_ where pred_bin ne . and recession ne .;
      quit;
      %let tp_hp_oo = &tp_hp_oo;
      %let tn_hp_oo = &tn_hp_oo;
      %let fp_hp_oo = &fp_hp_oo;
      %let fn_hp_oo = &fn_hp_oo;

      /* AUC OOS via logit auxiliaire */
      ods output Association=_hp_ao_;
      proc logistic data=_hp_oos_ descending;
         model recession = prob;
      run;
      ods output close;
      %let auc_hp_oos = .;
      proc sql noprint;
         select nValue2 into :auc_hp_oos trimmed
         from _hp_ao_ where Label2 = 'c';
      quit;
      %if %quote(&auc_hp_oos) = %then %let auc_hp_oos = .;

      proc append base=&panel_oos
         data=_hp_oos_(keep=pays date_dec prob recession rec_lo rec_hi) force;
      run;

      data _hp_row_;
         length pays $15 modele $30 scope $5;
         pays = "&pays"; modele = "&tag"; scope = "IS";
         TP = &tp_hp_is; TN = &tn_hp_is; FP = &fp_hp_is; FN = &fn_hp_is;
         auc = &auc_hp_is; pseudo_r2 = &r2_hp_is; output;
         pays = "&pays"; modele = "&tag"; scope = "OOS";
         TP = &tp_hp_oo; TN = &tn_hp_oo; FP = &fp_hp_oo; FN = &fn_hp_oo;
         auc = &auc_hp_oos; pseudo_r2 = .; output;
      run;
      proc append base=synthese data=_hp_row_ force; run;

      proc datasets library=work nolist;
         delete _hp_fit_ _hp_assoc_ _hp_glob_ _hp_ao_
                _hp_is_ _hp_is2_ _hp_oos_ _hp_oos_raw_
                _gihp_ _hp_row_;
      quit;

   %mend run_hpmodel;


   /* ----------------------------------------------------------
      LANCEMENT DES 4 MODELES PAR PAYS
   ---------------------------------------------------------- */

   %run_model(tag=Probit Simple, vars=spread_l4,
      link=probit, panel_is=panel_probit_simple, panel_oos=panel_oos_simple,
      data_is=train_&pays, data_train=train_&pays, data_oos=oos_&pays);

   %run_model(tag=Probit Enrichi, vars=spread_l4 petro_l4 cli_l4 rate_l4 etf_l4,
      link=probit, panel_is=panel_probit_enrich, panel_oos=panel_oos_enrich,
      data_is=train_&pays, data_train=train_&pays, data_oos=oos_&pays);

   %run_hpmodel(tag=HPLogit Simple,
      vars=spread_l4,
      panel_is=panel_logit_simple, panel_oos=panel_oos_logit_simple,
      data_is=train_&pays, data_train=train_&pays, data_oos=oos_&pays);

   %run_hpmodel(tag=HPLogit Enrichi,
      vars=spread_l4 petro_l4 cli_l4 rate_l4 etf_l4,
      panel_is=panel_logit_enrich, panel_oos=panel_oos_logit_enrich,
      data_is=train_&pays, data_train=train_&pays, data_oos=oos_&pays);

   proc datasets library=work nolist;
      delete _don_ _ps_ mk_&pays train_&pays oos_&pays;
   quit;

%mend analyse_pays;


/* ============================================================
   LANCEMENT DES 4 PAYS
   ============================================================ */
%analyse_pays(pays=US,        an_debut=1992, an_fin=2023);
%analyse_pays(pays=France,    an_debut=1992, an_fin=2023);
%analyse_pays(pays=Allemagne, an_debut=1992, an_fin=2023);
%analyse_pays(pays=UK,        an_debut=1992, an_fin=2023);


/* ============================================================
   Renommage Allemagne -> Germany
   ============================================================ */
%macro rename_pays(ds=);
   data &ds; set &ds;
      if pays = 'Allemagne' then pays = 'Germany';
   run;
%mend;
%rename_pays(ds=panel_spread);
%rename_pays(ds=panel_probit_simple);
%rename_pays(ds=panel_probit_enrich);
%rename_pays(ds=panel_oos_simple);
%rename_pays(ds=panel_oos_enrich);
%rename_pays(ds=panel_logit_simple);
%rename_pays(ds=panel_logit_enrich);
%rename_pays(ds=panel_oos_logit_simple);
%rename_pays(ds=panel_oos_logit_enrich);
data synthese; set synthese;
   if pays = 'Allemagne' then pays = 'Germany';
run;


/* ============================================================
   MACRO GRAPHIQUE
   ============================================================ */

%macro graphe(data=, titre=, color=steelblue, label_y=Recession Probability,
              min_y=0, max_y=1, legend_label=Curve, is_spread=0);

   %if &is_spread=1 %then %do;
      data &data;
         set &data;
         if rec_lo ne . and rec_hi ne . then do; ymin=-4.5; ymax=4.5; end;
         else do; ymin=.; ymax=.; end;
      run;
      proc sgpanel data=&data;
         panelby pays / rows=2 columns=2 novarname
                        headerattrs=(size=11 weight=bold) spacing=5;
         highlow x=date_dec low=ymin high=ymax /
                 fillattrs=(color=lightred) lineattrs=(color=lightred)
                 legendlabel="Recession" transparency=0.1;
         series x=date_dec y=spread /
                lineattrs=(color=steelblue thickness=1.5) legendlabel="Raw Spread";
         series x=date_dec y=lisse /
                lineattrs=(color=firebrick thickness=2)
                legendlabel="Kalman Smoothed Spread";
         refline 0 / axis=y lineattrs=(color=darkgray pattern=dash thickness=1);
         colaxis label="Year" values=(1992 to 2024 by 4) min=1992 max=2024 grid;
         rowaxis label="Yield Spread (%)" min=-4.5 max=4.5 grid;
         title "&titre";
         keylegend / position=bottom across=3 noborder;
      run;
   %end;
   %else %do;
      proc sgpanel data=&data;
         panelby pays / rows=2 columns=2 novarname
                        headerattrs=(size=11 weight=bold) spacing=5;
         highlow x=date_dec low=rec_lo high=rec_hi /
                 fillattrs=(color=lightred) lineattrs=(color=lightred)
                 legendlabel="Recession" transparency=0.1;
         series x=date_dec y=prob /
                lineattrs=(color=&color thickness=2) legendlabel="&legend_label";
         refline 0.5 / axis=y lineattrs=(color=black pattern=shortdash thickness=1);
         colaxis label="Year" grid;
         rowaxis label="&label_y" min=&min_y max=&max_y grid;
         title "&titre";
         keylegend / position=bottom across=2 noborder;
      run;
   %end;

%mend graphe;


/* ============================================================
   GRAPHIQUES 1 à 9
   ============================================================ */

%graphe(data=panel_spread, is_spread=1,
   titre=10-Year - 3-Month Yield Spread: Raw vs Kalman Smoothed -- 4 Countries (1992-2023));

%graphe(data=panel_probit_simple, color=steelblue,
   legend_label=Simple Probit,
   titre=Recession Probability IN-SAMPLE -- Simple Probit: spread (t-4) -- 4 Countries);

%graphe(data=panel_probit_enrich, color=firebrick,
   legend_label=Augmented Probit,
   titre=Recession Probability IN-SAMPLE -- Augmented Probit: spread+oil+CLI+rate+ETF (t-4) -- 4 Countries);

%graphe(data=panel_oos_simple, color=steelblue,
   legend_label=Simple Probit (OOS),
   titre=Recession Probability OUT-OF-SAMPLE -- Simple Probit -- 4 Countries (30%));

%graphe(data=panel_oos_enrich, color=firebrick,
   legend_label=Augmented Probit (OOS),
   titre=Recession Probability OUT-OF-SAMPLE -- Augmented Probit -- 4 Countries (30%));

%graphe(data=panel_logit_simple, color=darkgreen,
   legend_label=Simple HPLogit (ridge),
   titre=Recession Probability IN-SAMPLE -- Simple HPLogit (ridge NR): spread (t-4) -- 4 Countries);

%graphe(data=panel_logit_enrich, color=darkorange,
   legend_label=Augmented HPLogit (ridge),
   titre=Recession Probability IN-SAMPLE -- Augmented HPLogit (ridge NR): spread+oil+CLI+rate+ETF (t-4) -- 4 Countries);

%graphe(data=panel_oos_logit_simple, color=darkgreen,
   legend_label=Simple HPLogit OOS (ridge),
   titre=Recession Probability OUT-OF-SAMPLE -- Simple HPLogit (ridge NR) -- 4 Countries (30%));

%graphe(data=panel_oos_logit_enrich, color=darkorange,
   legend_label=Augmented HPLogit OOS (ridge),
   titre=Recession Probability OUT-OF-SAMPLE -- Augmented HPLogit (ridge NR) -- 4 Countries (30%));


/* ============================================================
   TABLEAU DE SYNTHESE
   ============================================================ */

proc sort data=synthese; by pays modele scope; run;

proc print data=synthese noobs label;
   var pays modele scope TP TN FP FN auc pseudo_r2;
   format auc pseudo_r2 8.4;
   label pays      = "Country"
         modele    = "Model"
         scope     = "Scope"
         TP        = "TP"
         TN        = "TN"
         FP        = "FP"
         FN        = "FN"
         auc       = "AUC (c)"
         pseudo_r2 = "R2 IS (Cox-Snell)";
   title1 "SUMMARY TABLE -- PROBIT & HPLOGIT (Simple / Augmented)";
   title2 "Simple: spread_{t-4} | Augmented: spread+oil+CLI+rate+ETF_{t-4}";
   title3 "Probit=PROC LOGISTIC (Fisher scoring) | HPLogit=PROC HPLOGISTIC (Newton-Raphson ridge)";
   title4 "IS: 70% train | OOS: 30% test | Period: 1992-2023 | 4 countries x 4 models";
run;
