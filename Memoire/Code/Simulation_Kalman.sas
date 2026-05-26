/* ============================================================
   PROJET : Prevision de crises financieres (Mishkin & Estrella)
   Donnees simulees -- 80 observations trimestrielles
   ============================================================ */

/* ============================================================
   ETAPE 1 -- GENERATION DES DONNEES SIMULEES
   Variables :
     - spread   : ecart taux 10 ans - taux 3 mois (%)
     - conf     : indice de confiance des menages
     - petrol   : variation du prix du petrole (%)
     - recession: variable binaire (1=recession, 0=expansion)
   ============================================================ */

data macro;
   call streaminit(42);
   do t = 1 to 80;
      spread_base = 1.5 - 0.03 * t + 0.4 * sin(t / 6);
      spread      = spread_base + rand('normal', 0, 0.3);
      conf   = 100 + rand('normal', 0, 8) - 5 * (spread < 0);
      petrol = rand('normal', 2, 10);
      /* FIX: tirets ASCII simples, pas Unicode minus */
      prob_rec  = 1 / (1 + exp(-(-1.5 - 2.0 * spread + 0.01 * (100 - conf))));
      recession = (rand('uniform') < prob_rec);
      output;
   end;
run;

proc print data=macro (obs=10);
   var t spread conf petrol recession;
   title "Extrait des donnees simulees (10 premieres observations)";
run;

proc means data=macro mean std min max;
   var spread conf petrol recession;
   title "Statistiques descriptives";
run;

proc freq data=macro;
   tables recession;
   title "Distribution de la variable recession";
run;


/* ============================================================
   ETAPE 2 -- FILTRAGE DE KALMAN SUR LE SPREAD
   ============================================================ */

data nile_spread;
   set macro;
   y = spread;
   keep t y;
run;

proc iml;
   reset log;
   use nile_spread; read all var {y};

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

   resu = (1:nrow(Y)-1)` || a[2:nrow(a)-1] || alpha[2:nrow(Y)] || y[2:nrow(Y)];
   print resu[colname={"t" "filtre" "lisse" "spread_obs"}];

   varnames = {"t" "filtre" "lisse" "spread_obs"};
   create kalman_out from resu[colname=varnames];
   append from resu;
   close kalman_out;
quit;

/* Fusion */
proc sort data=macro;      by t; run;
proc sort data=kalman_out; by t; run;

data macro_kalman;
   merge macro (in=a) kalman_out (keep=t lisse filtre);
   by t;
   if a;
   signal_kalman = (lisse < 0);
run;


/* ============================================================
   ETAPE 3 -- MODELE LOGIT DE BASE (Mishkin & Estrella)
   ============================================================ */

proc logistic data=macro_kalman descending;
   model recession = spread / lackfit rsq ctable pprob=0.5;
   output out=pred_base pred=prob_base;
   title "Modele logit de base -- Spread seul";
run;


/* ============================================================
   ETAPE 4 -- MODELE PROBIT DE BASE
   ============================================================ */

proc logistic data=macro_kalman descending;
   model recession = spread / link=probit lackfit rsq ctable pprob=0.5;
   output out=pred_probit pred=prob_probit;
   title "Modele probit de base -- Spread seul";
run;


/* ============================================================
   ETAPE 5 -- VALIDATION : MATRICE DE CONFUSION ET AUC
   ============================================================ */

/* FIX: on joint via t, pas par merge sans by */
proc sort data=pred_base;   by t; run;
proc sort data=pred_probit; by t; run;

data pred_all;
   merge macro_kalman (keep=t recession spread)
         pred_base    (keep=t prob_base)
         pred_probit  (keep=t prob_probit);
   by t;
   pred_logit   = (prob_base   >= 0.5);
   pred_probit2 = (prob_probit >= 0.5);
run;

proc freq data=pred_all;
   tables recession * pred_logit / norow nocol nopercent;
   title "Matrice de confusion -- Logit (seuil 0.5)";
run;

proc logistic data=macro_kalman descending;
   model recession = spread;
   roc;
   title "Courbe ROC -- Logit de base";
run;


/* ============================================================
   ETAPE 6 -- MODELE ENRICHI : SPREAD + VARIABLES MACRO
   ============================================================ */

proc logistic data=macro_kalman descending;
   model recession = spread conf petrol /
         selection=stepwise slentry=0.10 slstay=0.15
         lackfit rsq ctable pprob=0.5;
   output out=pred_enrich pred=prob_enrich;
   title "Modele logit enrichi -- Spread + Confiance + Petrole";
run;

/* FIX: comparaison ROC via les datasets de predictions */
proc sort data=pred_enrich; by t; run;

data roc_compare;
   merge macro_kalman (keep=t recession)
         pred_base    (keep=t prob_base)
         pred_enrich  (keep=t prob_enrich);
   by t;
run;

proc logistic data=roc_compare descending;
   model recession(event='1') = prob_base prob_enrich / nofit;
   roc "Base"    pred=prob_base;
   roc "Enrichi" pred=prob_enrich;
   roccontrast reference("Enrichi") / estimate all;
   title "Comparaison des courbes ROC : base vs enrichi";
run;


/* ============================================================
   ETAPE 7 -- MODELE AVEC SPREAD LISSE PAR KALMAN
   ============================================================ */

proc logistic data=macro_kalman descending;
   model recession = lisse conf petrol / lackfit rsq ctable pprob=0.5;
   output out=pred_kalman pred=prob_kalman;
   title "Modele logit -- Spread lisse (Kalman) + variables macro";
run;

proc sort data=pred_kalman; by t; run;

data roc_kalman;
   merge macro_kalman (keep=t recession)
         pred_base    (keep=t prob_base)
         pred_kalman  (keep=t prob_kalman);
   by t;
run;

proc logistic data=roc_kalman descending;
   model recession(event='1') = prob_base prob_kalman / nofit;
   roc "Base"         pred=prob_base;
   roc "Kalman+macro" pred=prob_kalman;
   roccontrast reference("Kalman+macro") / estimate all;
   title "ROC : Spread lisse (Kalman) vs Spread brut";
run;


/* ============================================================
   ETAPE 8 -- VALIDATION OUT-OF-SAMPLE
   In-sample  : t = 1 a 60
   Out-sample : t = 61 a 80
   ============================================================ */

data train oos;
   set macro_kalman;
   if t <= 60 then output train;
   else            output oos;
run;

/* FIX: store le modele dans un catalogue, pas outmodel= */
proc logistic data=train descending;
   model recession = spread conf petrol;
   store out=work.logit_model;
   title "Modele logit -- In-sample (t=1 a 60)";
run;

/* Scoring out-of-sample */
proc plm restore=work.logit_model;
   score data=oos out=score_oos predicted=prob_oos;
run;

data score_oos;
   set score_oos;
   pred_oos = (prob_oos >= 0.5);
run;

proc freq data=score_oos;
   tables recession * pred_oos / norow nocol nopercent;
   title "Matrice de confusion -- Out-of-sample (t=61 a 80)";
run;

proc logistic data=score_oos descending;
   model recession(event='1') = prob_oos / nofit;
   roc pred=prob_oos;
   title "AUC -- Out-of-sample";
run;


/* ============================================================
   ETAPE 9 -- GRAPHIQUE : Spread brut vs lisse + recessions
   ============================================================ */

data graph_data;
   set macro_kalman;
   if recession = 1 then do;
      lower = -3;
      upper =  3;
   end;
   else do;
      lower = .;
      upper = .;
   end;
run;

proc sgplot data=graph_data;
   band   x=t lower=lower upper=upper /
          transparency=0.6 legendlabel="Recession"
          fillattrs=(color=lightred);
   series x=t y=spread / lineattrs=(color=steelblue thickness=1.5)
          legendlabel="Spread brut";
   series x=t y=lisse  / lineattrs=(color=firebrick  thickness=2.5)
          legendlabel="Spread lisse (Kalman)";
   refline 0 / axis=y lineattrs=(color=black pattern=dash);
   xaxis label="Trimestre";
   yaxis label="Ecart de taux (%)";
   title "Spread brut vs spread lisse par filtre de Kalman";
   keylegend / position=bottom;
run;
