

libname inv "\\groups.ghc.org\data\CTRHS\MHRN\Suic Risk Supplement MHRN2\Programming\Analytic\COMBINED" access=readonly;
libname outv "\\groups\data\CTRHS\MHRN\Eric";


%include '\\home\johnex3\SAS\Scripts\sasntlogon.sas';
%include '\\ghcmaster\ghri\warehouse\remote\remotestart.sas';

libname inv "\\groups.ghc.org\data\CTRHS\MHRN\Suic Risk Supplement MHRN2\Programming\Analytic\COMBINED" access=readonly;
libname outv "\\groups\data\CTRHS\MHRN\Eric";
%macro dwork(indata, outdata);

data 
working0
working1
working2
working3
working4
working5
working6
working7
working8
working9; 
set &indata.;
length result $ 1;
result = left(reverse(person_id));
if result = "0" then output working0;
if result = "1" then output working1;
if result = "2" then output working2;
if result = "3" then output working3;
if result = "4" then output working4;
if result = "5" then output working5;
if result = "6" then output working6;
if result = "7" then output working7;
if result = "8" then output working8;
if result = "9" then output working9;
run;

%do i=0 %to 9;
data working&i.; set working&i.;
outcome_days = min(any_attempt_days, any_sui_dth_days, censor_att_days);
outcome_event = max(any_attempt_post, any_sui_dth_flag, 0);
if (missing(any_attempt_days)=0 or missing(any_sui_dth_days)=0) and min(any_attempt_days, any_sui_dth_days) > censor_att_days then outcome_event=0;

death_days = min(any_sui_dth_days, censor_dth_days);
death_event = max(any_sui_dth_flag, 0);
if any_sui_dth_days > censor_dth_days then death_event=0;

death30 = 0;
if death_days < 30 and death_event = 0 then death30 = .;
if death_days <= 30 and death_event = 1 then death30 = 1;

death90 = 0;
if death_days < 90 and death_event = 0 then death90 = .;
if death_days <= 90 and death_event = 1 then death90 = 1;



event30 = 0;
if outcome_days < 30 and outcome_event = 0 then event30 = .;
if outcome_days <= 30 and outcome_event = 1 then event30 = 1;

event90 = 0;
if outcome_days < 90 and outcome_event = 0 then event90 = .;
if outcome_days <= 90 and outcome_event = 1 then event90 = 1;


first_visit = missing(days_since_visit1);
if missing(days_since_visit1) then days_since_visit1=0;
if missing(days_since_prev) then days_since_prev = 0;
length raceeth $ 40;
raceeth = race1;
if race1 in ("WH", "UN", "OT", "BA") and hispanic = "Y" then raceeth = "Hispanic";
if raceeth in ("MU", "UN" "OT") then raceeth = "Other/Unknown";
if raceeth = "AS" then raceeth = "Asian";
if raceeth = "BA" then raceeth = "Black";
if raceeth = "HP" then raceeth = "HA/Pacific Islander";
if raceeth = "IN" then raceeth = "American Indian";
if raceeth = "WH" then raceeth = "White";
length ins_type $ 25;
ins_type = "Unknown";
if ins_medicaid = "Y" then ins_type = "Medicaid";
else if ins_commercial = "Y" then ins_type = "Commercial";
else if ins_privatepay = "Y" then ins_type = "Private Pay";
else if ins_statesubsidized = "Y" then ins_type = "State Subsidized";
else if ins_selffunded = "Y" then ins_type = "Self-Funded";
else if ins_medicare = "Y" then ins_type = "Medicare";
else if ins_other = "Y" then ins_type = "Other";
else if ins_highdeductible = "Y" then ins_type = "High Deductible";

length agecat $ 20;
if (0 <= age <= 17) then agecat = "13-17";
if (18 <= age <= 29) then agecat = "18-29";
if (30 <= age <= 44) then agecat = "30-44";
if (45 <= age <= 64) then agecat = "45-64";
if (65 <= age <= 200) then agecat = "65+";


ac1 = (agecat = "13-17");
ac2 = (agecat = "18-29");
ac3 = (agecat = "30-44");
ac4 = (agecat = "45-64");
ac5 = (agecat = "65+");

ac1f = (agecat = "13-17") * (sex="F");
ac2f = (agecat = "18-29") * (sex="F");
ac3f = (agecat = "30-44") * (sex="F");
ac4f = (agecat = "45-64") * (sex="F");
ac5f = (agecat = "65+") * (sex="F");

notEnrolled = missing(enr_pre_days);
if missing(enr_pre_days) then enr_pre_days = 0;

del_post_1_180 = max(del_post_1_90, del_post_91_180);
del_post_1_280 = max(del_post_1_90, del_post_91_180, del_post_181_280);
del_pre_1_180 = max(del_pre_1_90, del_pre_91_180);
del_pre_1_365 = max(del_pre_1_90, del_pre_91_180, del_pre_181_365);

if missing(del_post_1_90) then del_post_1_90 = 0;
if missing(del_post_1_180) then del_post_1_180 = 0;
if missing(del_post_1_280) then del_post_1_280 = 0;
if missing(del_pre_1_90) then del_pre_1_90 = 0;
if missing(del_pre_1_180) then del_pre_1_180 = 0;
if missing(del_pre_1_365) then del_pre_1_365 = 0;

dep_dx_pre5y_cumulative = max(dep_dx_index, dep_dx_pre1y, dep_dx_pre5y);
dep_dx_pre5y_noi_cumulative = max(dep_dx_pre1y, dep_dx_pre5y);

anx_dx_pre5y_cumulative = max(anx_dx_index, anx_dx_pre1y, anx_dx_pre5y);
anx_dx_pre5y_noi_cumulative = max(anx_dx_pre1y, anx_dx_pre5y);

bip_dx_pre5y_cumulative = max(bip_dx_index, bip_dx_pre1y, bip_dx_pre5y);
bip_dx_pre5y_noi_cumulative = max(bip_dx_pre1y, bip_dx_pre5y);

sch_dx_pre5y_cumulative = max(sch_dx_index, sch_dx_pre1y, sch_dx_pre5y);
sch_dx_pre5y_noi_cumulative = max(sch_dx_pre1y, sch_dx_pre5y);

oth_dx_pre5y_cumulative = max(oth_dx_index, oth_dx_pre1y, oth_dx_pre5y);
oth_dx_pre5y_noi_cumulative = max(oth_dx_pre1y, oth_dx_pre5y);

dem_dx_pre5y_cumulative = max(dem_dx_index, dem_dx_pre1y, dem_dx_pre5y);
dem_dx_pre5y_noi_cumulative = max(dem_dx_pre1y, dem_dx_pre5y);

add_dx_pre5y_cumulative = max(add_dx_index, add_dx_pre1y, add_dx_pre5y);
add_dx_pre5y_noi_cumulative = max(add_dx_pre1y, add_dx_pre5y);

asd_dx_pre5y_cumulative = max(asd_dx_index, asd_dx_pre1y, asd_dx_pre5y);
asd_dx_pre5y_noi_cumulative = max(asd_dx_pre1y, asd_dx_pre5y);

per_dx_pre5y_cumulative = max(per_dx_index, per_dx_pre1y, per_dx_pre5y);
per_dx_pre5y_noi_cumulative = max(per_dx_pre1y, per_dx_pre5y);

alc_dx_pre5y_cumulative = max(alc_dx_index, alc_dx_pre1y, alc_dx_pre5y);
alc_dx_pre5y_noi_cumulative = max(alc_dx_pre1y, alc_dx_pre5y);

dru_dx_pre5y_cumulative = max(dru_dx_index, dru_dx_pre1y, dru_dx_pre5y);
dru_dx_pre5y_noi_cumulative = max(dru_dx_pre1y, dru_dx_pre5y);

pts_dx_pre5y_cumulative = max(pts_dx_index, pts_dx_pre1y, pts_dx_pre5y);
pts_dx_pre5y_noi_cumulative = max(pts_dx_pre1y, pts_dx_pre5y);

eat_dx_pre5y_cumulative = max(eat_dx_index, eat_dx_pre1y, eat_dx_pre5y);
eat_dx_pre5y_noi_cumulative = max(eat_dx_pre1y, eat_dx_pre5y);

tbi_dx_pre5y_cumulative = max(tbi_dx_index, tbi_dx_pre1y, tbi_dx_pre5y);
tbi_dx_pre5y_noi_cumulative = max(tbi_dx_pre1y, tbi_dx_pre5y);

antidep_rx_pre1y_cumulative = max(antidep_rx_pre3m, antidep_rx_pre1y);
antidep_rx_pre5y_cumulative = max(antidep_rx_pre3m, antidep_rx_pre1y, antidep_rx_pre5y);

benzo_rx_pre1y_cumulative = max(benzo_rx_pre3m, benzo_rx_pre1y);
benzo_rx_pre5y_cumulative = max(benzo_rx_pre3m, benzo_rx_pre1y, benzo_rx_pre5y);

hypno_rx_pre1y_cumulative = max(hypno_rx_pre3m, hypno_rx_pre1y);
hypno_rx_pre5y_cumulative = max(hypno_rx_pre3m, hypno_rx_pre1y, hypno_rx_pre5y);

sga_rx_pre1y_cumulative = max(sga_rx_pre3m, sga_rx_pre1y);
sga_rx_pre5y_cumulative = max(sga_rx_pre3m, sga_rx_pre1y, sga_rx_pre5y);

mh_ip_pre1y_cumulative = max(mh_ip_pre3m, mh_ip_pre1y);
mh_ip_pre5y_cumulative = max(mh_ip_pre3m, mh_ip_pre1y, mh_ip_pre5y);

mh_op_pre1y_cumulative = max(mh_op_pre3m, mh_op_pre1y);
mh_op_pre5y_cumulative = max(mh_op_pre3m, mh_op_pre1y, mh_op_pre5y);

mh_ed_pre1y_cumulative = max(mh_ed_pre3m, mh_ed_pre1y);
mh_ed_pre5y_cumulative = max(mh_ed_pre3m, mh_ed_pre1y, mh_ed_pre5y);

any_sui_att_pre1y_cumulative = max(any_sui_att_pre3m, any_sui_att_pre1y);
any_sui_att_pre5y_cumulative = max(any_sui_att_pre3m, any_sui_att_pre1y, any_sui_att_pre5y);

lvi_sui_att_pre1y_cumulative = max(lvi_sui_att_pre3m, lvi_sui_att_pre1y);
lvi_sui_att_pre5y_cumulative = max(lvi_sui_att_pre3m, lvi_sui_att_pre1y, lvi_sui_att_pre5y);

ovi_sui_att_pre1y_cumulative = max(ovi_sui_att_pre3m, ovi_sui_att_pre1y);
ovi_sui_att_pre5y_cumulative = max(ovi_sui_att_pre3m, ovi_sui_att_pre1y, ovi_sui_att_pre5y);

any_inj_poi_pre1y_cumulative = max(any_inj_poi_pre3m, any_inj_poi_pre1y);
any_inj_poi_pre5y_cumulative = max(any_inj_poi_pre3m, any_inj_poi_pre1y, any_inj_poi_pre5y);


array c_array(*) charlson_score charlson_mi charlson_chd charlson_pvd charlson_cvd charlson_dem charlson_cpd charlson_rhd charlson_pud charlson_mlivd charlson_diab charlson_diabc charlson_plegia charlson_ren charlson_malign charlson_slivd charlson_mst charlson_aids;
do i=1 to dim(c_array);
if missing(c_array(i)) then c_array(i) = 0;
if enr_pre_days < 365 then c_array(i) = 0;
end;
charlson_missing = (enr_pre_days < 365);
if missing(item9_index_score) then item9_index_score = -9;

phq8_index_score_calc = mean(item1_index_score, item2_index_score, item3_index_score, item4_index_score, item5_index_score, item6_index_score, item7_index_score, item8_index_score) * 8;
if missing(phq8_index_score_calc) then phq8_missing = 1; else phq8_missing=0;
if missing(phq8_index_score_calc) then phq8_index_score_calc = 0;
run;


proc sql;
create table temp as select distinct person_id, days_since_visit1, item9_index_score from working&i. order by person_id, days_since_visit1;
quit; run;
proc sql;
create table temp2 as select distinct a.*, b.days_since_visit1 as days_since_visit1_2, b.item9_index_score as item9_index_score_2
from temp as a left join temp as b on a.person_id = b.person_id;
quit; run;

proc sql;
create table temp3 as select distinct person_id, days_since_visit1, item9_index_score,
sum( (0 < (days_since_visit1 - days_since_visit1_2) <= 90) * (item9_index_score_2 = 0) ) as prior9_90_0,
sum( (0 < (days_since_visit1 - days_since_visit1_2) <= 90) * (item9_index_score_2 = 1) ) as prior9_90_1,
sum( (0 < (days_since_visit1 - days_since_visit1_2) <= 90) * (item9_index_score_2 = 2) ) as prior9_90_2,
sum( (0 < (days_since_visit1 - days_since_visit1_2) <= 90) * (item9_index_score_2 = 3) ) as prior9_90_3,

sum( (0 < (days_since_visit1 - days_since_visit1_2) <= 183) * (item9_index_score_2 = 0) ) as prior9_183_0,
sum( (0 < (days_since_visit1 - days_since_visit1_2) <= 183) * (item9_index_score_2 = 1) ) as prior9_183_1,
sum( (0 < (days_since_visit1 - days_since_visit1_2) <= 183) * (item9_index_score_2 = 2) ) as prior9_183_2,
sum( (0 < (days_since_visit1 - days_since_visit1_2) <= 183) * (item9_index_score_2 = 3) ) as prior9_183_3,

sum( (0 < (days_since_visit1 - days_since_visit1_2) <= 365) * (item9_index_score_2 = 0) ) as prior9_365_0,
sum( (0 < (days_since_visit1 - days_since_visit1_2) <= 365) * (item9_index_score_2 = 1) ) as prior9_365_1,
sum( (0 < (days_since_visit1 - days_since_visit1_2) <= 365) * (item9_index_score_2 = 2) ) as prior9_365_2,
sum( (0 < (days_since_visit1 - days_since_visit1_2) <= 365) * (item9_index_score_2 = 3) ) as prior9_365_3
from temp2
group by person_id, days_since_visit1
order by person_id, days_since_visit1;
quit; run;

proc sql;
create table working&i. as select a.*, b.* from working&i. as a left join temp3 as b on a.person_id = b.person_id and a.days_since_visit1 = b.days_since_visit1 order by a.person_id, a.days_since_visit1;
quit; run;

/*for each timepoint, need phqNone, phqNumber, phqMode (0 if none), phqMax*/
data working&i.; set working&i.;
phqNumber90 = sum(prior9_90_0, prior9_90_1, prior9_90_2, prior9_90_3);
phqMissing90 = (phqNumber90=0);
if phqNumber90 = 0 then phqMode90 = 0;
else if prior9_90_3 >= max(prior9_90_0, prior9_90_1, prior9_90_2) then phqMode90 = 3;
else if prior9_90_2 >= max(prior9_90_0, prior9_90_1) then phqMode90 = 2;
else if prior9_90_1 >= max(prior9_90_0) then phqMode90 = 1;
else phqMode90 = 0;
phqMax90 = max( (prior9_90_0>0)*0, (prior9_90_1>0)*1, (prior9_90_2>0)*2, (prior9_90_3>0)*3 );

phqNumber183 = sum(prior9_183_0, prior9_183_1, prior9_183_2, prior9_183_3);
phqMissing183 = (phqNumber183=0);
if phqNumber183 = 0 then phqMode183 = 0;
else if prior9_183_3 >= max(prior9_183_0, prior9_183_1, prior9_183_2) then phqMode183 = 3;
else if prior9_183_2 >= max(prior9_183_0, prior9_183_1) then phqMode183 = 2;
else if prior9_183_1 >= max(prior9_183_0) then phqMode183 = 1;
else phqMode183 = 0;
phqMax183 = max( (prior9_183_0>0)*0, (prior9_183_1>0)*1, (prior9_183_2>0)*2, (prior9_183_3>0)*3 );

phqNumber365 = sum(prior9_365_0, prior9_365_1, prior9_365_2, prior9_365_3);
phqMissing365 = (phqNumber365=0);
if phqNumber365 = 0 then phqMode365 = 0;
else if prior9_365_3 >= max(prior9_365_0, prior9_365_1, prior9_365_2) then phqMode365 = 3;
else if prior9_365_2 >= max(prior9_365_0, prior9_365_1) then phqMode365 = 2;
else if prior9_365_1 >= max(prior9_365_0) then phqMode365 = 1;
else phqMode365 = 0;
phqMax365 = max( (prior9_365_0>0)*0, (prior9_365_1>0)*1, (prior9_365_2>0)*2, (prior9_365_3>0)*3 );




proc sql;
create table working&i._analytic as
select distinct person_id, visit_seq,
event30, 
event90,
death30,
death90,
visit_type = "MH" as visit_mh,
/*site*/
/*year*/
/*Model 1 variables: Demographics, insurance, utilization and meds prior to visit*/
age, 
ac1,
ac3,
ac4,
ac5,
ac1f,
ac3f,
ac4f,
ac5f,
notEnrolled,

coalesce(ins_medicaid="Y", 0) as medicaid,
coalesce(ins_commercial="Y", 0) as commercial,
coalesce(ins_privatepay="Y", 0) as privatepay,
coalesce(ins_statesubsidized="Y", 0) as statesubsidized,
coalesce(ins_selffunded="Y", 0) as selffunded,
coalesce(ins_medicare="Y", 0) as medicare,
/*ins_other*/
coalesce(ins_highdeductible="Y", 0) as highdeductible,
first_visit,
days_since_prev,
sex="F" as female,


dep_dx_pre5y_noi_cumulative,
dep_dx_pre5y,
anx_dx_pre5y_noi_cumulative,
anx_dx_pre5y,
bip_dx_pre5y_noi_cumulative,
bip_dx_pre5y,
sch_dx_pre5y_noi_cumulative,
sch_dx_pre5y,
oth_dx_pre5y_noi_cumulative,
oth_dx_pre5y,
dem_dx_pre5y_noi_cumulative,
dem_dx_pre5y,
add_dx_pre5y_noi_cumulative,
add_dx_pre5y,
asd_dx_pre5y_noi_cumulative,
asd_dx_pre5y,
per_dx_pre5y_noi_cumulative,
per_dx_pre5y,
alc_dx_pre5y_noi_cumulative,
alc_dx_pre5y,
pts_dx_pre5y_noi_cumulative,
pts_dx_pre5y,
eat_dx_pre5y_noi_cumulative,
eat_dx_pre5y,
tbi_dx_pre5y_noi_cumulative,
tbi_dx_pre5y,
dru_dx_pre5y_noi_cumulative,
dru_dx_pre5y,

antidep_rx_pre3m,
antidep_rx_pre1y_cumulative,
antidep_rx_pre5y_cumulative,
benzo_rx_pre3m,
benzo_rx_pre1y_cumulative,
benzo_rx_pre5y_cumulative,
hypno_rx_pre3m,
hypno_rx_pre1y_cumulative,
hypno_rx_pre5y_cumulative,
sga_rx_pre3m,
sga_rx_pre1y_cumulative,
sga_rx_pre5y_cumulative,
mh_ip_pre3m,
mh_ip_pre1y_cumulative,
mh_ip_pre5y_cumulative,
mh_op_pre3m,
mh_op_pre1y_cumulative,
mh_op_pre5y_cumulative,
mh_ed_pre3m,
mh_ed_pre1y_cumulative,
mh_ed_pre5y_cumulative,
any_sui_att_pre3m,
any_sui_att_pre1y_cumulative,
any_sui_att_pre5y_cumulative,
lvi_sui_att_pre3m,
lvi_sui_att_pre1y_cumulative,
lvi_sui_att_pre5y_cumulative,
ovi_sui_att_pre3m,
ovi_sui_att_pre1y_cumulative,
ovi_sui_att_pre5y_cumulative,
any_inj_poi_pre3m,
any_inj_poi_pre1y_cumulative,
any_inj_poi_pre5y_cumulative,
max(del_post_1_90, del_post_1_180) as current_pregnancy,
del_pre_1_90,
del_pre_1_180,
del_pre_1_365,

any_sui_att_pre5y_cumulative * (sex="F") as any_sui_att_pre5y_cumulative_f,
any_sui_att_pre5y_cumulative * age as any_sui_att_pre5y_cumulative_a,

charlson_score,
charlson_score * age as charlson_a,
charlson_mi,
charlson_chd,
charlson_pvd,
charlson_cvd,
charlson_dem,
charlson_cpd,
charlson_rhd,
charlson_pud,
charlson_mlivd,
charlson_diab,
charlson_diabc,
charlson_plegia,
charlson_ren,
charlson_malign,
charlson_slivd,
charlson_mst,
charlson_aids,

/*Model 2 variables: Race/Ethnicity*/

race1="AS" and any_sui_att_pre5y_cumulative as raceAsian_asa,
race1="BA" and any_sui_att_pre5y_cumulative as raceBlack_asa,
race1="HP" and any_sui_att_pre5y_cumulative as raceHP_asa,
race1="IN" and any_sui_att_pre5y_cumulative as raceIN_asa,
(race1 in ("MU", "OT")) and any_sui_att_pre5y_cumulative as raceMUOT_asa,
race1="UN" and any_sui_att_pre5y_cumulative as raceUN_asa,
coalesce(hispanic="Y", 0) and any_sui_att_pre5y_cumulative as hispanic_asa,



race1="AS" as raceAsian,
race1="BA" as raceBlack,
race1="HP" as raceHP,
race1="IN" as raceIN,
race1 in ("MU", "OT") as raceMUOT,
race1="UN" as raceUN,
coalesce(hispanic="Y", 0) as hispanic,

race1="AS" and sex="F" as raceAsian_f,
race1="BA" and sex="F" as raceBlack_f,
race1="HP" and sex="F" as raceHP_f,
race1="IN" and sex="F" as raceIN_f,
race1 in ("MU", "OT") and sex="F" as raceMUOT_f,
race1="UN" and sex="F" as raceUN_f,
coalesce(hispanic="Y", 0) and sex="F" as hispanic_f,

/*Model 3 variables: Census/ACA*/
missing(hhld_inc_lt40k) as census_missing,
coalesce(hhld_inc_lt40k, 0) as hhld_inc_lt40k,
coalesce(coll_deg_lt25p, 0) as coll_deg_lt25p,

/*Model 4 variables: Prior PHQ visits*/
	/*prior phq information*/
phqNumber90,
(phqMissing90=0)*(phqMode90=0) as phqMode90_0,
(phqMissing90=0)*(phqMode90=1) as phqMode90_1,
(phqMissing90=0)*(phqMode90=2) as phqMode90_2,
/*(phqMissing90=0)*(phqMode90=3) as phqMode90_3,*/
(phqMissing90=0)*(phqMax90=0) as phqMax90_0,
(phqMissing90=0)*(phqMax90=1) as phqMax90_1,
(phqMissing90=0)*(phqMax90=2) as phqMax90_2,
(phqMissing90=0)*(phqMax90=3) as phqMax90_3,

phqNumber183,
(phqMissing183=0)*(phqMode183=0) as phqMode183_0,
(phqMissing183=0)*(phqMode183=1) as phqMode183_1,
(phqMissing183=0)*(phqMode183=2) as phqMode183_2,
/*(phqMissing183=0)*(phqMode183=3) as phqMode183_3,*/
(phqMissing183=0)*(phqMax183=0) as phqMax183_0,
(phqMissing183=0)*(phqMax183=1) as phqMax183_1,
(phqMissing183=0)*(phqMax183=2) as phqMax183_2,
(phqMissing183=0)*(phqMax183=3) as phqMax183_3,

phqNumber365,
(phqMissing365=0)*(phqMode365=0) as phqMode365_0,
(phqMissing365=0)*(phqMode365=1) as phqMode365_1,
(phqMissing365=0)*(phqMode365=2) as phqMode365_2,
/*(phqMissing365=0)*(phqMode365=3) as phqMode365_3,*/
(phqMissing365=0)*(phqMax365=0) as phqMax365_0,
(phqMissing365=0)*(phqMax365=1) as phqMax365_1,
(phqMissing365=0)*(phqMax365=2) as phqMax365_2,
(phqMissing365=0)*(phqMax365=3) as phqMax365_3,





/* Model 5 variables: Information from date of exam*/


race1="AS" and dep_dx_pre5y_cumulative=1 as raceAsian_de,
race1="BA" and dep_dx_pre5y_cumulative=1 as raceBlack_de,
race1="HP" and dep_dx_pre5y_cumulative=1 as raceHP_de,
race1="IN" and dep_dx_pre5y_cumulative=1 as raceIN_de,
race1 in ("MU", "OT") and dep_dx_pre5y_cumulative=1 as raceMUOT_de,
race1="UN" and dep_dx_pre5y_cumulative=1 as raceUN_de,
coalesce(hispanic="Y", 0) and dep_dx_pre5y_cumulative=1 as hispanic_de,

race1="AS" and anx_dx_pre5y_cumulative=1 as raceAsian_an,
race1="BA" and anx_dx_pre5y_cumulative=1 as raceBlack_an,
race1="HP" and anx_dx_pre5y_cumulative=1 as raceHP_an,
race1="IN" and anx_dx_pre5y_cumulative=1 as raceIN_an,
race1 in ("MU", "OT") and anx_dx_pre5y_cumulative=1 as raceMUOT_an,
race1="UN" and anx_dx_pre5y_cumulative=1 as raceUN_an,
coalesce(hispanic="Y", 0) and anx_dx_pre5y_cumulative=1 as hispanic_an,

race1="AS" and bip_dx_pre5y_cumulative=1 as raceAsian_bi,
race1="BA" and bip_dx_pre5y_cumulative=1 as raceBlack_bi,
race1="HP" and bip_dx_pre5y_cumulative=1 as raceHP_bi,
race1="IN" and bip_dx_pre5y_cumulative=1 as raceIN_bi,
race1 in ("MU", "OT") and bip_dx_pre5y_cumulative=1 as raceMUOT_bi,
race1="UN" and bip_dx_pre5y_cumulative=1 as raceUN_bi,
coalesce(hispanic="Y", 0) and bip_dx_pre5y_cumulative=1 as hispanic_bi,

race1="AS" and sch_dx_pre5y_cumulative=1 as raceAsian_sc,
race1="BA" and sch_dx_pre5y_cumulative=1 as raceBlack_sc,
race1="HP" and sch_dx_pre5y_cumulative=1 as raceHP_sc,
race1="IN" and sch_dx_pre5y_cumulative=1 as raceIN_sc,
race1 in ("MU", "OT") and sch_dx_pre5y_cumulative=1 as raceMUOT_sc,
race1="UN" and sch_dx_pre5y_cumulative=1 as raceUN_sc,
coalesce(hispanic="Y", 0) and sch_dx_pre5y_cumulative=1 as hispanic_sc,

dep_dx_pre5y_cumulative,
dep_dx_pre5y_cumulative * (sex="F") as dep_dx_pre5y_cumulative_f,
dep_dx_pre5y_cumulative * age as dep_dx_pre5y_cumulative_a,

anx_dx_pre5y_cumulative,
anx_dx_pre5y_cumulative * (sex="F") as anx_dx_pre5y_cumulative_f,
anx_dx_pre5y_cumulative * age as anx_dx_pre5y_cumulative_a,

bip_dx_pre5y_cumulative,
bip_dx_pre5y_cumulative * (sex="F") as bip_dx_pre5y_cumulative_f,
bip_dx_pre5y_cumulative * age as bip_dx_pre5y_cumulative_a,

sch_dx_pre5y_cumulative,
sch_dx_pre5y_cumulative * (sex="F") as sch_dx_pre5y_cumulative_f,
sch_dx_pre5y_cumulative * age as sch_dx_pre5y_cumulative_a,

oth_dx_pre5y_cumulative,
dem_dx_pre5y_cumulative,
add_dx_pre5y_cumulative,
asd_dx_pre5y_cumulative,
per_dx_pre5y_cumulative,
alc_dx_pre5y_cumulative,
dru_dx_pre5y_cumulative,
pts_dx_pre5y_cumulative,
eat_dx_pre5y_cumulative,
tbi_dx_pre5y_cumulative,

	/*current phq information*/
phq8_index_score_calc,
phq8_missing,
phq8_index_score_calc * (sex="F") as phq8_index_score_calc_f,
phq8_missing * (sex="F") as phq8_missing_f,

phq8_index_score_calc * (race1="AS") as raceAsian_8,
phq8_index_score_calc * (race1="BA") as raceBlack_8,
phq8_index_score_calc * (race1="HP") as raceHP_8,
phq8_index_score_calc * (race1="IN") as raceIN_8,
phq8_index_score_calc * (race1 in ("MU", "OT")) as raceMUOT_8,
phq8_index_score_calc * (race1="UN") as raceUN_8,
phq8_index_score_calc * (coalesce(hispanic="Y", 0)) as hispanic_8,

phq8_index_score_calc * age as age_8,

(item9_index_score=0) as q9_0,
(item9_index_score=1) as q9_1,
(item9_index_score=2) as q9_2,
(item9_index_score=3) as q9_3,

(item9_index_score=0) and (sex="F") as q9_0_f,
(item9_index_score=1) and (sex="F") as q9_1_f,
(item9_index_score=2) and (sex="F") as q9_2_f,
(item9_index_score=3) and (sex="F") as q9_3_f,

(item9_index_score=0) * age as q9_0_a,
(item9_index_score=1) * age as q9_1_a,
(item9_index_score=2) * age as q9_2_a,
(item9_index_score=3) * age as q9_3_a,

race1="AS" and item9_index_score=0 as raceAsian_q90,
race1="BA" and item9_index_score=0 as raceBlack_q90,
race1="HP" and item9_index_score=0 as raceHP_q90,
race1="IN" and item9_index_score=0 as raceIN_q90,
(race1 in ("MU", "OT")) and item9_index_score=0 as raceMUOT_q90,
race1="UN" and item9_index_score=0 as raceUN_q90,
coalesce(hispanic="Y", 0) and item9_index_score=0 as hispanic_q90,

race1="AS" and item9_index_score=1 as raceAsian_q91,
race1="BA" and item9_index_score=1 as raceBlack_q91,
race1="HP" and item9_index_score=1 as raceHP_q91,
race1="IN" and item9_index_score=1 as raceIN_q91,
(race1 in ("MU", "OT")) and item9_index_score=1 as raceMUOT_q91,
race1="UN" and item9_index_score=1 as raceUN_q91,
coalesce(hispanic="Y", 0) and item9_index_score=1 as hispanic_q91,

race1="AS" and item9_index_score=2 as raceAsian_q92,
race1="BA" and item9_index_score=2 as raceBlack_q92,
race1="HP" and item9_index_score=2 as raceHP_q92,
race1="IN" and item9_index_score=2 as raceIN_q92,
race1 in ("MU", "OT") and item9_index_score=2 as raceMUOT_q92,
race1="UN" and item9_index_score=2 as raceUN_q92,
coalesce(hispanic="Y", 0) and item9_index_score=2 as hispanic_q92,

race1="AS" and item9_index_score=3 as raceAsian_q93,
race1="BA" and item9_index_score=3 as raceBlack_q93,
race1="HP" and item9_index_score=3 as raceHP_q93,
race1="IN" and item9_index_score=3 as raceIN_q93,
race1 in ("MU", "OT") and item9_index_score=3 as raceMUOT_q93,
race1="UN" and item9_index_score=3 as raceUN_q93,
coalesce(hispanic="Y", 0) and item9_index_score=3 as hispanic_q93,

(item9_index_score=0) * phq8_index_score_calc as q9_0_8,
(item9_index_score=1) * phq8_index_score_calc as q9_1_8,
(item9_index_score=2) * phq8_index_score_calc as q9_2_8,
(item9_index_score=3) * phq8_index_score_calc as q9_3_8,

(item9_index_score=0) * charlson_score as q9_0_c,
(item9_index_score=1) * charlson_score as q9_1_c,
(item9_index_score=2) * charlson_score as q9_2_c,
(item9_index_score=3) * charlson_score as q9_3_c,

(item9_index_score=0) * dep_dx_pre5y_cumulative as q9_0_de,
(item9_index_score=1) * dep_dx_pre5y_cumulative as q9_1_de,
(item9_index_score=2) * dep_dx_pre5y_cumulative as q9_2_de,
(item9_index_score=3) * dep_dx_pre5y_cumulative as q9_3_de,

(item9_index_score=0) * anx_dx_pre5y_cumulative as q9_0_an,
(item9_index_score=1) * anx_dx_pre5y_cumulative as q9_1_an,
(item9_index_score=2) * anx_dx_pre5y_cumulative as q9_2_an,
(item9_index_score=3) * anx_dx_pre5y_cumulative as q9_3_an,

(item9_index_score=0) * bip_dx_pre5y_cumulative as q9_0_bi,
(item9_index_score=1) * bip_dx_pre5y_cumulative as q9_1_bi,
(item9_index_score=2) * bip_dx_pre5y_cumulative as q9_2_bi,
(item9_index_score=3) * bip_dx_pre5y_cumulative as q9_3_bi,

(item9_index_score=0) * sch_dx_pre5y_cumulative as q9_0_sc,
(item9_index_score=1) * sch_dx_pre5y_cumulative as q9_1_sc,
(item9_index_score=2) * sch_dx_pre5y_cumulative as q9_2_sc,
(item9_index_score=3) * sch_dx_pre5y_cumulative as q9_3_sc,

(item9_index_score=0) * alc_dx_pre5y_cumulative as q9_0_al,
(item9_index_score=1) * alc_dx_pre5y_cumulative as q9_1_al,
(item9_index_score=2) * alc_dx_pre5y_cumulative as q9_2_al,
(item9_index_score=3) * alc_dx_pre5y_cumulative as q9_3_al,

(item9_index_score=0) * dru_dx_pre5y_cumulative as q9_0_dr,
(item9_index_score=1) * dru_dx_pre5y_cumulative as q9_1_dr,
(item9_index_score=2) * dru_dx_pre5y_cumulative as q9_2_dr,
(item9_index_score=3) * dru_dx_pre5y_cumulative as q9_3_dr,

(item9_index_score=0) * per_dx_pre5y_cumulative as q9_0_pe,
(item9_index_score=1) * per_dx_pre5y_cumulative as q9_1_pe,
(item9_index_score=2) * per_dx_pre5y_cumulative as q9_2_pe,
(item9_index_score=3) * per_dx_pre5y_cumulative as q9_3_pe,



any_sui_att_pre5y_cumulative * phq8_index_score_calc as any_sui_att_pre5y_cumulative_8,
any_sui_att_pre5y_cumulative * charlson_score as any_sui_att_pre5y_cumulative_c,


any_sui_att_pre5y_cumulative * dep_dx_pre5y_cumulative as any_sui_att_pre5y_cumulative_de,
any_sui_att_pre5y_cumulative * anx_dx_pre5y_cumulative as any_sui_att_pre5y_cumulative_an,
any_sui_att_pre5y_cumulative * bip_dx_pre5y_cumulative as any_sui_att_pre5y_cumulative_bi,
any_sui_att_pre5y_cumulative * sch_dx_pre5y_cumulative as any_sui_att_pre5y_cumulative_sc,
any_sui_att_pre5y_cumulative * alc_dx_pre5y_cumulative as any_sui_att_pre5y_cumulative_al,
any_sui_att_pre5y_cumulative * dru_dx_pre5y_cumulative as any_sui_att_pre5y_cumulative_dr,
any_sui_att_pre5y_cumulative * per_dx_pre5y_cumulative as any_sui_att_pre5y_cumulative_pe,


/*prior phq information, interaction*/

(phqMissing90=0)*(phqMax90=0)*(item9_index_score=0) as phqMax90_0_q90,
(phqMissing90=0)*(phqMax90=1)*(item9_index_score=0) as phqMax90_1_q90,
(phqMissing90=0)*(phqMax90=2)*(item9_index_score=0) as phqMax90_2_q90,
(phqMissing90=0)*(phqMax90=3)*(item9_index_score=0) as phqMax90_3_q90,

(phqMissing90=0)*(phqMax90=0)*(item9_index_score=1) as phqMax90_0_q91,
(phqMissing90=0)*(phqMax90=1)*(item9_index_score=1) as phqMax90_1_q91,
(phqMissing90=0)*(phqMax90=2)*(item9_index_score=1) as phqMax90_2_q91,
(phqMissing90=0)*(phqMax90=3)*(item9_index_score=1) as phqMax90_3_q91,

(phqMissing90=0)*(phqMax90=0)*(item9_index_score=2) as phqMax90_0_q92,
(phqMissing90=0)*(phqMax90=1)*(item9_index_score=2) as phqMax90_1_q92,
(phqMissing90=0)*(phqMax90=2)*(item9_index_score=2) as phqMax90_2_q92,
(phqMissing90=0)*(phqMax90=3)*(item9_index_score=2) as phqMax90_3_q92,

(phqMissing90=0)*(phqMax90=0)*(item9_index_score=3) as phqMax90_0_q93,
(phqMissing90=0)*(phqMax90=1)*(item9_index_score=3) as phqMax90_1_q93,
(phqMissing90=0)*(phqMax90=2)*(item9_index_score=3) as phqMax90_2_q93,
(phqMissing90=0)*(phqMax90=3)*(item9_index_score=3) as phqMax90_3_q93


/*maybe good enough for now?  phqMax183  phqMax365 left out of interactions*/

from working&i.
order by person_id, visit_seq; 
quit; run;
%end;


data 
&outdata.; 
set 
working0_analytic 
working1_analytic 
working2_analytic 
working3_analytic 
working4_analytic 
working5_analytic 
working6_analytic 
working7_analytic 
working8_analytic 
working9_analytic; 
length cvvar $ 1;
cvvar = substr(reverse(person_id), 2, 1);
run;
%mend;
%dwork(inv.srs2_analytic_combined, outv.analytic_all);
endrsubmit;
signoff ghridwip;


data 
outv.valid_pc90
outv.analytic_pc90
outv.valid_mh90
outv.analytic_mh90
; set outv.analytic_all;
call streaminit(123);
u = rand("Uniform");
if u <= 0.35 and visit_mh=0 then output outv.valid_pc90;
if u >  0.35 and visit_mh=0 then output outv.analytic_pc90;
if u <= 0.35 and visit_mh=1 then output outv.valid_mh90;
if u >  0.35 and visit_mh=1 then output outv.analytic_mh90;
drop current_pregnancy del_pre_1_90 del_pre_1_180 del_pre_1_365 visit_mh u;
run;


PROC EXPORT DATA= outv.analytic_mh90
            OUTFILE= "\\groups\data\CTRHS\MHRN\Eric\analytic_mh90.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;
PROC EXPORT DATA= outv.analytic_pc90
            OUTFILE= "\\groups\data\CTRHS\MHRN\Eric\analytic_pc90.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;

PROC EXPORT DATA= outv.valid_mh90
            OUTFILE= "\\groups\data\CTRHS\MHRN\Eric\valid_mh90.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;
PROC EXPORT DATA= outv.valid_pc90
            OUTFILE= "\\groups\data\CTRHS\MHRN\Eric\valid_pc90.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;
