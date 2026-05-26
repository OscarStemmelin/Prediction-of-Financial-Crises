*****************************************;
** SAS Scoring Code for PROC Hplogistic;
*****************************************;

length I_recession $ 12;
label I_recession = 'Vers : recession' ;
label U_recession = 'Non normalisé dans : recession' ;
format U_recession BEST12.0;

label P_recession1 = 'Valeur prédite : recession=1' ;
label P_recession0 = 'Valeur prédite : recession=0' ;

drop _LMR_BAD;
_LMR_BAD=0;

*** Check interval variables for missing values;
if nmiss(spread_l4,petro_l4,cli_l4,rate_l4,etf_l4) then do;
   _LMR_BAD=1;
   goto _SKIP_000;
end;

*** Compute Linear Predictors;
drop _LP0;
_LP0 = 0;

*** Effect: spread_l4;
_LP0 = _LP0 + (-84.5850075248777) * spread_l4;
*** Effect: petro_l4;
_LP0 = _LP0 + (7.16821242115967) * petro_l4;
*** Effect: cli_l4;
_LP0 = _LP0 + (-285.638131321093) * cli_l4;
*** Effect: rate_l4;
_LP0 = _LP0 + (1.16346809658643) * rate_l4;
*** Effect: etf_l4;
_LP0 = _LP0 + (0.21698337839544) * etf_l4;

*** Predicted values;
drop _MAXP _IY _P0 _P1;
_TEMP = -234.981671591911  + _LP0;
if (_TEMP < 0) then do;
   _TEMP = exp(_TEMP);
   _P0 = _TEMP / (1 + _TEMP);
end;
else _P0 = 1 / (1 + exp(-_TEMP));
_P1 = 1.0 - _P0;
P_recession1 = _P0;
_MAXP = _P0;
_IY = 1;
P_recession0 = _P1;
if (_P1 >  _MAXP + 1E-8) then do;
   _MAXP = _P1;
   _IY = 2;
end;
select( _IY );
   when (1) do;
      I_recession = '1' ;
      U_recession = 1;
   end;
   when (2) do;
      I_recession = '0' ;
      U_recession = 0;
   end;
   otherwise do;
      I_recession = '';
      U_recession = .;
   end;
end;
_SKIP_000:
if _LMR_BAD = 1 then do;
I_recession = '';
U_recession = .;
P_recession1 = .;
P_recession0 = .;
end;
drop _TEMP;
