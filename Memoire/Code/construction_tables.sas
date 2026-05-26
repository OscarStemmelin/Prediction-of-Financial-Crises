/* ============================================================
   CONSTRUCTION DES TABLES DE DONNEES PAR PAYS
   Variables : recession, spread_brut, spread_kalman,
               var_petrole, var_cli, var_etf, rate_dir
   Taux directeur EUR : Bundesbank lombard (1992-1998)
                        + DFR BCE (1999-2023)
   Export : Donnees_US.csv, Donnees_France.csv, etc.
   ============================================================ */

%let path = C:\Users\oscar\Desktop\Memoire;


/* ============================================================
   MACRO UTILITAIRE : import + renommage colonne 2
   ============================================================ */

%macro import_renomme(fichier=, type=);
   proc import datafile="&path\&fichier..csv"
       out=_raw_ dbms=csv replace;
       getnames=yes;
   run;
   proc contents data=_raw_ out=_cols_ noprint; run;
   proc sql noprint;
      select name into :col2 from _cols_ where varnum = 2;
   quit;
   data _raw_;
      set _raw_;
      rename &col2 = &type observation_date = obs_date;
   run;
   data &fichier._std; set _raw_; run;
%mend;


/* ============================================================
   SERIES COMMUNES : PETROLE + ETF
   ============================================================ */

/* --- Petrole (variation trimestrielle %) --- */
%import_renomme(fichier=Prix_petrole, type=petrole);

data _petro_m_;
   set Prix_petrole_std;
   annee = year(obs_date); trim = qtr(obs_date);
   if petrole ne .; keep annee trim petrole;
run;
proc means data=_petro_m_ noprint;
   class annee trim; var petrole;
   output out=_petro_q_ (where=(_type_=3)) mean=petro_moy;
run;
proc sort data=_petro_q_; by annee trim; run;
data petrole_q;
   set _petro_q_;
   petro_lag = lag(petro_moy);
   if petro_lag ne . and petro_lag > 0 then
      var_petrole = (petro_moy - petro_lag) / petro_lag * 100;
   keep annee trim var_petrole;
run;

/* --- ETF boursiers (variation trimestrielle %) --- */
proc import datafile="&path\ETF.csv"
    out=etf_raw dbms=csv replace;
    getnames=yes;
run;
data etf_raw; set etf_raw; rename Date = obs_date; run;
proc sort data=etf_raw; by obs_date; run;

data etf_q;
   set etf_raw;
   annee = year(obs_date); trim = qtr(obs_date);
   sp500_lag   = lag(SP_500);
   cac40_lag   = lag(CAC_40);
   dax40_lag   = lag(DAX_40);
   ftse100_lag = lag(FTSE_100);
   if sp500_lag   > 0 then var_etf_US = (SP_500   - sp500_lag)   / sp500_lag   * 100;
   if cac40_lag   > 0 then var_etf_FR = (CAC_40   - cac40_lag)   / cac40_lag   * 100;
   if dax40_lag   > 0 then var_etf_DE = (DAX_40   - dax40_lag)   / dax40_lag   * 100;
   if ftse100_lag > 0 then var_etf_UK = (FTSE_100 - ftse100_lag) / ftse100_lag * 100;
   keep annee trim var_etf_US var_etf_FR var_etf_DE var_etf_UK;
run;
proc sort data=etf_q; by annee trim; run;


/* ============================================================
   MACRO PRINCIPALE : construction table pays
   Parametres :
     pays     : US / France / Allemagne / UK
     mode     : US (spread direct + recession NBER)
              ou FR (spread calcule t10-t3 + recession via PIB)
     f_rate   : dataset taux directeur trimestriel
     f_etf    : colonne ETF dans etf_q (var_etf_US etc.)
   ============================================================ */

%macro construire_pays(pays=, mode=FR,
                       f_spread=, f_rec=,
                       f_t10=, f_t3=, f_pib=,
                       f_cli=, f_rate=, f_etf=,
                       an_debut=1992, an_fin=2023);

   %put ===== Construction &pays =====;


   /* ----------------------------------------------------------
      SPREAD ET RECESSION
   ---------------------------------------------------------- */

   %if &mode = US %then %do;

      %import_renomme(fichier=&f_spread, type=spread_j);
      %import_renomme(fichier=&f_rec,    type=usrec);

      data _sm_;
         set &f_spread._std;
         annee=year(obs_date); trim=qtr(obs_date);
         if spread_j ne .; keep annee trim spread_j;
      run;
      proc means data=_sm_ noprint;
         class annee trim; var spread_j;
         output out=_sq_ (where=(_type_=3)) mean=spread_brut;
      run;

      data _rm_;
         set &f_rec._std;
         annee=year(obs_date); trim=qtr(obs_date);
         keep annee trim usrec;
      run;
      proc means data=_rm_ noprint;
         class annee trim; var usrec;
         output out=_rq_ (where=(_type_=3)) max=recession;
      run;

   %end;

   %if &mode = FR %then %do;

      %import_renomme(fichier=&f_t10, type=t10);
      %import_renomme(fichier=&f_t3,  type=t3);
      %import_renomme(fichier=&f_pib, type=pib);

      data _t10m_;
         set &f_t10._std; annee=year(obs_date); trim=qtr(obs_date);
         if t10 ne .; keep annee trim t10;
      run;
      proc means data=_t10m_ noprint;
         class annee trim; var t10;
         output out=_t10q_ (where=(_type_=3)) mean=t10;
      run;

      data _t3m_;
         set &f_t3._std; annee=year(obs_date); trim=qtr(obs_date);
         if t3 ne .; keep annee trim t3;
      run;
      proc means data=_t3m_ noprint;
         class annee trim; var t3;
         output out=_t3q_ (where=(_type_=3)) mean=t3;
      run;

      proc sort data=_t10q_; by annee trim; run;
      proc sort data=_t3q_;  by annee trim; run;

      data _sq_;
         merge _t10q_ (keep=annee trim t10) _t3q_ (keep=annee trim t3);
         by annee trim;
         if t10 ne . and t3 ne .;
         spread_brut = t10 - t3;
         keep annee trim spread_brut;
      run;

      /* Recession : 2 trimestres consecutifs de croissance negative */
      data _pib_;
         set &f_pib._std; annee=year(obs_date); trim=qtr(obs_date);
         keep annee trim pib;
      run;
      proc sort data=_pib_; by annee trim; run;
      data _pib_;
         set _pib_;
         pib_lag = lag(pib);
         if pib_lag ne . and pib_lag > 0 then
            croissance = (pib - pib_lag) / pib_lag * 100;
      run;
      data _rq_;
         set _pib_;
         croiss_lag = lag(croissance);
         if croissance ne . and croiss_lag ne . then
            recession = (croissance < 0 and croiss_lag < 0);
         keep annee trim recession;
      run;

   %end;


   /* ----------------------------------------------------------
      CLI (variation mensuelle -> trimestrielle)
   ---------------------------------------------------------- */

   %import_renomme(fichier=&f_cli, type=cli_raw);

   data _clim_;
      set &f_cli._std;
      annee=year(obs_date); trim=qtr(obs_date);
      cli_m=cli_raw; if cli_m ne .;
      keep annee trim obs_date cli_m;
   run;
   proc sort data=_clim_; by obs_date; run;
   data _clim_;
      set _clim_;
      cli_lag1 = lag(cli_m);
      if cli_lag1 ne . then var_cli = cli_m - cli_lag1;
      keep annee trim var_cli;
   run;
   proc means data=_clim_ noprint;
      class annee trim; var var_cli;
      output out=_cliq_ (where=(_type_=3)) mean=var_cli;
   run;


   /* ----------------------------------------------------------
      TAUX DIRECTEUR
   ---------------------------------------------------------- */

   data _rate_;
      set &f_rate (keep=annee trim rate_dir);
   run;


   /* ----------------------------------------------------------
      ETF du pays
   ---------------------------------------------------------- */

   data _etf_;
      set etf_q (keep=annee trim &f_etf);
      var_etf = &f_etf;
      keep annee trim var_etf;
   run;


   /* ----------------------------------------------------------
      FUSION
   ---------------------------------------------------------- */

   proc sort data=_sq_;   by annee trim; run;
   proc sort data=_rq_;   by annee trim; run;
   proc sort data=_cliq_; by annee trim; run;
   proc sort data=_rate_; by annee trim; run;
   proc sort data=_etf_;  by annee trim; run;
   proc sort data=petrole_q; by annee trim; run;

   data _fusion_;
      merge _sq_      (keep=annee trim spread_brut)
            _rq_      (keep=annee trim recession)
            petrole_q (keep=annee trim var_petrole)
            _cliq_    (keep=annee trim var_cli)
            _rate_    (keep=annee trim rate_dir)
            _etf_     (keep=annee trim var_etf);
      by annee trim;
      if spread_brut ne . and recession ne .
         and annee >= &an_debut and annee <= &an_fin;
   run;


   /* ----------------------------------------------------------
      FILTRE DE KALMAN SUR LE SPREAD BRUT
      -> produit spread_kalman (lisse) et spread_filtre
   ---------------------------------------------------------- */

   data _nile_;
      set _fusion_;
      y = spread_brut;
      t + 1;
      keep t y;
   run;

   proc iml;
      reset log;
      use _nile_; read all var {y};

      start kalman(var) global(y);
         V = j(nrow(Y), 1, 0);
         F = j(nrow(Y), 1, 0);
         K = j(nrow(Y), 1, 0);
         A = j(nrow(Y)+1, 1, 0);
         P = j(nrow(Y)+1, 1, 10e7);
         do t = 1 to nrow(Y);
            v[t] = y[t] - a[t];
            f[t] = p[t] + exp(var[1]);
            k[t] = p[t] / f[t];
            a[t+1] = a[t] + k[t] * v[t];
            p[t+1] = p[t] * (1 - k[t]) + exp(var[2]);
         end;
         t1 = 0;
         do i = 2 to nrow(Y);
            t1 = t1 + log(f[i]) + v[i]*v[i]/f[i];
         end;
         ld1 = -(nrow(Y)/2) * log(8*atan(1)) - 0.5 * t1;
         return(Ld1);
      finish kalman;

      parm = 1||1;
      opt  = {1 2 . 4};
      call NLPQN(rc, Ldv, "kalman", parm, opt);

      /* Filtrage */
      var = ldv;
      V = j(nrow(Y), 1, 0);
      F = j(nrow(Y), 1, 0);
      K = j(nrow(Y), 1, 0);
      A = j(nrow(Y)+1, 1, 0);
      P = j(nrow(Y)+1, 1, 10e7);
      L = j(nrow(Y), 1, 0);
      do t = 1 to nrow(Y);
         v[t] = y[t] - a[t];
         f[t] = p[t] + exp(var[1]);
         k[t] = p[t] / F[t];
         L[t] = 1 - k[t];
         a[t+1] = a[t] + k[t] * v[t];
         p[t+1] = p[t] * (1 - k[t]) + exp(var[2]);
      end;

      /* Lissage */
      r     = j(nrow(Y), 1, 0);
      alpha = j(nrow(Y), 1, 0);
      do t = -nrow(Y) to -2;
         t1 = abs(t);
         r[t1-1]   = inv(F[t1])*v[t1] + L[t1]*r[t1];
         alpha[t1] = a[t1] + P[t1]*r[t1-1];
      end;
      rr       = inv(F[1])*v[1] + L[1]*r[1];
      alpha[1] = a[1] + P[1]*rr;

      resu = (1:nrow(Y)-1)` || a[2:nrow(a)-1] || alpha[2:nrow(Y)];
      varnames={"t" "spread_filtre" "spread_kalman"};
      create _kalman_ from resu[colname=varnames];
      append from resu;
      close _kalman_;
   quit;

   /* Ajout d'un t dans _fusion_ pour le merge */
   data _fusion_;
      set _fusion_;
      t + 1;
   run;

   proc sort data=_fusion_;  by t; run;
   proc sort data=_kalman_;  by t; run;

   data Donnees_&pays;
      merge _fusion_ (in=a) _kalman_ (keep=t spread_filtre spread_kalman);
      by t; if a;
      annee_trim = cats(annee, 'Q', trim);
      label annee_trim     = "Trimestre"
            recession      = "Recession (0/1)"
            spread_brut    = "Spread brut 10ans-3mois (%)"
            spread_kalman  = "Spread lisse Kalman (%)"
            spread_filtre  = "Spread filtre Kalman (%)"
            var_petrole    = "Variation prix petrole (%)"
            var_cli        = "Variation CLI (points)"
            var_etf        = "Variation indice boursier (%)"
            rate_dir       = "Taux directeur (%)";
      keep annee trim annee_trim recession
           spread_brut spread_kalman spread_filtre
           var_petrole var_cli var_etf rate_dir;
   run;

   %put Donnees_&pays : %sysfunc(attrn(%sysfunc(open(Donnees_&pays)),nobs)) observations;

   /* Export CSV */
   proc export data=Donnees_&pays
      outfile="&path\Donnees_&pays..csv"
      dbms=csv replace;
   run;

   %put Export OK : &path\Donnees_&pays..csv;

   /* Nettoyage intermediaire */
   proc datasets library=work nolist;
      delete _sm_ _sq_ _rm_ _rq_ _pib_ _t10m_ _t10q_
             _t3m_ _t3q_ _clim_ _cliq_ _rate_ _etf_
             _nile_ _kalman_ _fusion_;
   quit;

%mend construire_pays;


/* ============================================================
   IMPORT TAUX DIRECTEURS
   ============================================================ */

/* --- Fed Funds Rate -> US --- */
%import_renomme(fichier=Taux_directeurs_Fed, type=fedfunds);
data _ffm_;
   set Taux_directeurs_Fed_std;
   annee=year(obs_date); trim=qtr(obs_date);
   if fedfunds ne .;
   keep annee trim fedfunds;
   rename fedfunds = rate_m;
run;
proc means data=_ffm_ noprint;
   class annee trim; var rate_m;
   output out=fedfunds_q (where=(_type_=3)) mean=rate_dir;
run;
proc sort data=fedfunds_q; by annee trim; run;


/* ============================================================
   TAUX DIRECTEUR ZONE EURO : Bundesbank (1992-1998) + DFR BCE (1999+)
   Bundesbank : taux lombard mensuel, fichier format special
                (9 lignes de metadonnees, puis YYYY-MM,valeur,flag)
   DFR BCE    : depot facility rate journalier depuis 1999-01-01
   ============================================================ */

/* --- Bundesbank lombard rate (mensuel, 1992-1998) --- */
data _buba_raw_;
   infile "&path\Taux_Directeur_Bundesbank.csv"
          dsd dlm=',' firstobs=10 truncover missover;
   length date_str $10 flag_str $30;
   input date_str $ rate_buba : ?? best12. flag_str $;
   /* Garde uniquement les lignes de donnees au format YYYY-MM */
   if length(strip(date_str)) = 7
      and char(strip(date_str), 5) = '-'
      and input(substr(strip(date_str), 1, 4), ?? 4.) ne .
      and rate_buba ne .;
   annee = input(substr(strip(date_str), 1, 4), 4.);
   mois  = input(substr(strip(date_str), 6, 2), 2.);
   trim  = ceil(mois / 3);
   if 1992 <= annee <= 1998;
   keep annee trim rate_buba;
run;

proc means data=_buba_raw_ noprint;
   class annee trim; var rate_buba;
   output out=_buba_q_ (where=(_type_=3)) mean=rate_dir;
run;
proc sort data=_buba_q_; by annee trim; run;

/* --- DFR BCE (journalier, depuis 1999-01-01) --- */
%import_renomme(fichier=DFR_BCE, type=ecbdfr);
data _dfr_m_;
   set DFR_BCE_std;
   annee=year(obs_date); trim=qtr(obs_date);
   if ecbdfr ne .;
   keep annee trim ecbdfr;
   rename ecbdfr = rate_m;
run;
proc means data=_dfr_m_ noprint;
   class annee trim; var rate_m;
   output out=_dfr_q_ (where=(_type_=3)) mean=rate_dir;
run;
proc sort data=_dfr_q_; by annee trim; run;

/* --- Serie combinee : Bundesbank avant 1999, DFR BCE a partir de 1999 --- */
data eur_rate_q;
   set _buba_q_ (keep=annee trim rate_dir where=(annee < 1999))
       _dfr_q_  (keep=annee trim rate_dir where=(annee >= 1999));
run;
proc sort data=eur_rate_q; by annee trim; run;

%put Serie eur_rate_q : %sysfunc(attrn(%sysfunc(open(eur_rate_q)),nobs)) trimestres;


/* ============================================================
   LANCEMENT POUR LES 4 PAYS
   ============================================================ */

%construire_pays(
   pays    = US,
   mode    = US,
   f_spread = Spread_US,
   f_rec    = Recession_US,
   f_cli    = CLI_US,
   f_rate   = fedfunds_q,
   f_etf    = var_etf_US,
   an_debut = 1992, an_fin = 2023
);

%construire_pays(
   pays    = France,
   mode    = FR,
   f_t10   = Taux_10ans_France,
   f_t3    = Taux_3mois_France,
   f_pib   = Pib_France,
   f_cli   = CLI_France,
   f_rate  = eur_rate_q,
   f_etf   = var_etf_FR,
   an_debut = 1992, an_fin = 2023
);

%construire_pays(
   pays    = Allemagne,
   mode    = FR,
   f_t10   = Taux_10ans_Allemagne,
   f_t3    = Taux_3mois_Allemagne,
   f_pib   = Pib_Allemagne,
   f_cli   = CLI_Allemagne,
   f_rate  = eur_rate_q,
   f_etf   = var_etf_DE,
   an_debut = 1992, an_fin = 2023
);

%construire_pays(
   pays    = UK,
   mode    = FR,
   f_t10   = Taux_10ans_UK,
   f_t3    = Taux_3mois_UK,
   f_pib   = Pib_UK,
   f_cli   = CLI_UK,
   f_rate  = eur_rate_q,
   f_etf   = var_etf_UK,
   an_debut = 1992, an_fin = 2023
);


/* ============================================================
   VERIFICATION FINALE
   ============================================================ */

%macro verif(pays=);
   %put --- Donnees_&pays :
   %sysfunc(attrn(%sysfunc(open(Donnees_&pays)),nobs)) obs,
   %sysfunc(attrn(%sysfunc(open(Donnees_&pays)),nvars)) variables;
%mend;
%verif(pays=US);
%verif(pays=France);
%verif(pays=Allemagne);
%verif(pays=UK);
