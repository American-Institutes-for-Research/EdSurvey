g3dREF <- c(
  "Call: gap(variable = \"mmat\", data = fin4.11, groupA = asbg01 == \"BOY\", ",
  "    achievementLevel = c(\"Intermediate International Benchmark\"), ",
  "    achievementDiscrete = TRUE, returnVarEstInputs = TRUE)",
  "",
  "Labels:",
  " group      definition nFullData nUsed",
  "     A asbg01 == \"BOY\"      5201  2361",
  "     B       \"default\"      5201  4603",
  "",
  "Percentage:",
  "     pctA  pctAse pctB pctBse  diffAB covAB diffABse diffABpValue  dofAB",
  " 51.31531 0.78797  100      0 -48.685     0  0.78797    < 2.2e-16 147.17",
  "",
  "Results:",
  "                        achievementLevel estimateA estimateAse estimateB estimateBse diffAB  covAB diffABse diffABpValue  dofAB",
  " At Intermediate International Benchmark     32.69       1.619    35.191      1.1679 -2.501 1.3872   1.1003      0.02635 64.609"
)

g2pREF <- c(
  "gapList",
  "Call: gap(variable = \"mmat\", data = fin4.11, groupA = asbg01 == \"BOY\", ",
  "    groupB = asbg01 == \"GIRL\", percentiles = c(50, 90), pctMethod = \"symmetric\")",
  "",
  "Labels:",
  " group       definition",
  "     A  asbg01 == \"BOY\"",
  "     B asbg01 == \"GIRL\"",
  "",
  "Percentage:",
  "     pctA  pctAse   pctB  pctBse diffAB   covAB diffABse diffABpValue  dofAB",
  " 51.31531 0.78797 48.685 0.78797 2.6306 -0.6209   1.5759       0.0972 147.17",
  "",
  "Results:",
  " percentiles estimateA estimateAse estimateB estimateBse  diffAB   covAB diffABse diffABpValue   dofAB",
  "          50    554.13      2.3721    544.59      3.3983  9.5439 0.20975   4.0934      0.02078 189.359",
  "          90    637.27      4.8821    624.32      4.8653 12.9494 0.25707   6.8551      0.06200  93.198"
)

g1alREF <- c(
  "Call: gap(variable = \"mmat\", data = fin4.11, groupA = asbg01 == \"BOY\", ",
  "    groupB = asbg01 == \"GIRL\", achievementLevel = \"Low International Benchmark\", ",
  "    achievementDiscrete = TRUE)",
  "",
  "Labels:",
  " group       definition nFullData nUsed",
  "     A  asbg01 == \"BOY\"      5201  2361",
  "     B asbg01 == \"GIRL\"      5201  2242",
  "",
  "Percentage:",
  "     pctA  pctAse   pctB  pctBse diffAB   covAB diffABse diffABpValue  dofAB",
  " 51.31531 0.78797 48.685 0.78797 2.6306 -0.6209   1.5759       0.0972 147.17",
  "",
  "Results:",
  "               achievementLevel estimateA estimateAse estimateB estimateBse  diffAB  covAB diffABse diffABpValue  dofAB",
  " At Low International Benchmark    12.604      1.3798    13.961       1.039 -1.3577 0.2823   1.5552       0.3897 29.594"
)

g1eqREF <- c(
  "Call: gap(variable = \"asbg07d\", data = fin4.11, groupA = asbg01 == ",
  "    \"BOY\", groupB = asbg01 == \"GIRL\", targetLevel = \"ONCE OR TWICE A WEEK\")",
  "",
  "Labels:",
  " group       definition nFullData nUsed",
  "     A  asbg01 == \"BOY\"      5201  2345",
  "     B asbg01 == \"GIRL\"      5201  2231",
  "",
  "Percentage:",
  "     pctA pctAse   pctB pctBse diffAB    covAB diffABse diffABpValue  dofAB",
  " 51.26581 0.7927 48.734 0.7927 2.5316 -0.62837   1.5854       0.1125 146.05",
  "",
  "Results:",
  " estimateA estimateAse estimateB estimateBse  diffAB   covAB diffABse diffABpValue  dofAB",
  "  20.82701      1.0812    22.476      1.0234 -1.6491 2.6e-05   1.4887       0.2703 114.42"
)

swREF <- c(
  "There are 4 full sample weights in this edsurvey.data.frame:",
  "  'totwgt' with 150 JK replicate weights (the default).", "    Jackknife replicate weight variables associated with the full sample weight 'totwgt':",
  "    'jk1', 'jk2', 'jk3', 'jk4', 'jk5', 'jk6', 'jk7', 'jk8', 'jk9', 'jk10', 'jk11', 'jk12', 'jk13', 'jk14', 'jk15', 'jk16', 'jk17', 'jk18', 'jk19', 'jk20', 'jk21', 'jk22', 'jk23', 'jk24', 'jk25', 'jk26', 'jk27', 'jk28', 'jk29', 'jk30', 'jk31', 'jk32', 'jk33', 'jk34', 'jk35', 'jk36', 'jk37', 'jk38', 'jk39', 'jk40', 'jk41', 'jk42', 'jk43', 'jk44', 'jk45', 'jk46', 'jk47', 'jk48', 'jk49', 'jk50', 'jk51', 'jk52', 'jk53', 'jk54', 'jk55', 'jk56',",
  "    'jk57', 'jk58', 'jk59', 'jk60', 'jk61', 'jk62', 'jk63', 'jk64', 'jk65', 'jk66', 'jk67', 'jk68', 'jk69', 'jk70', 'jk71', 'jk72', 'jk73', 'jk74', 'jk75', 'jk76', 'jk77', 'jk78', 'jk79', 'jk80', 'jk81', 'jk82', 'jk83', 'jk84', 'jk85', 'jk86', 'jk87', 'jk88', 'jk89', 'jk90', 'jk91', 'jk92', 'jk93', 'jk94', 'jk95', 'jk96', 'jk97', 'jk98', 'jk99', 'jk100', 'jk101', 'jk102', 'jk103', 'jk104', 'jk105', 'jk106', 'jk107', 'jk108', 'jk109', 'jk110',",
  "    'jk111', 'jk112', 'jk113', 'jk114', 'jk115', 'jk116', 'jk117', 'jk118', 'jk119', 'jk120', 'jk121', 'jk122', 'jk123', 'jk124', 'jk125', 'jk126', 'jk127', 'jk128', 'jk129', 'jk130', 'jk131', 'jk132', 'jk133', 'jk134', 'jk135', 'jk136', 'jk137', 'jk138', 'jk139', 'jk140', 'jk141', 'jk142', 'jk143', 'jk144', 'jk145', 'jk146', 'jk147', 'jk148', 'jk149', and 'jk150'",
  "",
  "  'tchwgt' with 150 JK replicate weights.", "    Jackknife replicate weight variables associated with the full sample weight 'tchwgt':",
  "    'jk.tchwgt1', 'jk.tchwgt2', 'jk.tchwgt3', 'jk.tchwgt4', 'jk.tchwgt5', 'jk.tchwgt6', 'jk.tchwgt7', 'jk.tchwgt8', 'jk.tchwgt9', 'jk.tchwgt10', 'jk.tchwgt11', 'jk.tchwgt12', 'jk.tchwgt13', 'jk.tchwgt14', 'jk.tchwgt15', 'jk.tchwgt16', 'jk.tchwgt17', 'jk.tchwgt18', 'jk.tchwgt19', 'jk.tchwgt20', 'jk.tchwgt21', 'jk.tchwgt22', 'jk.tchwgt23', 'jk.tchwgt24', 'jk.tchwgt25', 'jk.tchwgt26', 'jk.tchwgt27', 'jk.tchwgt28', 'jk.tchwgt29', 'jk.tchwgt30',",
  "    'jk.tchwgt31', 'jk.tchwgt32', 'jk.tchwgt33', 'jk.tchwgt34', 'jk.tchwgt35', 'jk.tchwgt36', 'jk.tchwgt37', 'jk.tchwgt38', 'jk.tchwgt39', 'jk.tchwgt40', 'jk.tchwgt41', 'jk.tchwgt42', 'jk.tchwgt43', 'jk.tchwgt44', 'jk.tchwgt45', 'jk.tchwgt46', 'jk.tchwgt47', 'jk.tchwgt48', 'jk.tchwgt49', 'jk.tchwgt50', 'jk.tchwgt51', 'jk.tchwgt52', 'jk.tchwgt53', 'jk.tchwgt54', 'jk.tchwgt55', 'jk.tchwgt56', 'jk.tchwgt57', 'jk.tchwgt58', 'jk.tchwgt59',",
  "    'jk.tchwgt60', 'jk.tchwgt61', 'jk.tchwgt62', 'jk.tchwgt63', 'jk.tchwgt64', 'jk.tchwgt65', 'jk.tchwgt66', 'jk.tchwgt67', 'jk.tchwgt68', 'jk.tchwgt69', 'jk.tchwgt70', 'jk.tchwgt71', 'jk.tchwgt72', 'jk.tchwgt73', 'jk.tchwgt74', 'jk.tchwgt75', 'jk.tchwgt76', 'jk.tchwgt77', 'jk.tchwgt78', 'jk.tchwgt79', 'jk.tchwgt80', 'jk.tchwgt81', 'jk.tchwgt82', 'jk.tchwgt83', 'jk.tchwgt84', 'jk.tchwgt85', 'jk.tchwgt86', 'jk.tchwgt87', 'jk.tchwgt88',",
  "    'jk.tchwgt89', 'jk.tchwgt90', 'jk.tchwgt91', 'jk.tchwgt92', 'jk.tchwgt93', 'jk.tchwgt94', 'jk.tchwgt95', 'jk.tchwgt96', 'jk.tchwgt97', 'jk.tchwgt98', 'jk.tchwgt99', 'jk.tchwgt100', 'jk.tchwgt101', 'jk.tchwgt102', 'jk.tchwgt103', 'jk.tchwgt104', 'jk.tchwgt105', 'jk.tchwgt106', 'jk.tchwgt107', 'jk.tchwgt108', 'jk.tchwgt109', 'jk.tchwgt110', 'jk.tchwgt111', 'jk.tchwgt112', 'jk.tchwgt113', 'jk.tchwgt114', 'jk.tchwgt115', 'jk.tchwgt116',",
  "    'jk.tchwgt117', 'jk.tchwgt118', 'jk.tchwgt119', 'jk.tchwgt120', 'jk.tchwgt121', 'jk.tchwgt122', 'jk.tchwgt123', 'jk.tchwgt124', 'jk.tchwgt125', 'jk.tchwgt126', 'jk.tchwgt127', 'jk.tchwgt128', 'jk.tchwgt129', 'jk.tchwgt130', 'jk.tchwgt131', 'jk.tchwgt132', 'jk.tchwgt133', 'jk.tchwgt134', 'jk.tchwgt135', 'jk.tchwgt136', 'jk.tchwgt137', 'jk.tchwgt138', 'jk.tchwgt139', 'jk.tchwgt140', 'jk.tchwgt141', 'jk.tchwgt142', 'jk.tchwgt143',",
  "    'jk.tchwgt144', 'jk.tchwgt145', 'jk.tchwgt146', 'jk.tchwgt147', 'jk.tchwgt148', 'jk.tchwgt149', and 'jk.tchwgt150'",
  "",
  "  'matwgt' with 150 JK replicate weights.", "    Jackknife replicate weight variables associated with the full sample weight 'matwgt':",
  "    'jk.matwgt1', 'jk.matwgt2', 'jk.matwgt3', 'jk.matwgt4', 'jk.matwgt5', 'jk.matwgt6', 'jk.matwgt7', 'jk.matwgt8', 'jk.matwgt9', 'jk.matwgt10', 'jk.matwgt11', 'jk.matwgt12', 'jk.matwgt13', 'jk.matwgt14', 'jk.matwgt15', 'jk.matwgt16', 'jk.matwgt17', 'jk.matwgt18', 'jk.matwgt19', 'jk.matwgt20', 'jk.matwgt21', 'jk.matwgt22', 'jk.matwgt23', 'jk.matwgt24', 'jk.matwgt25', 'jk.matwgt26', 'jk.matwgt27', 'jk.matwgt28', 'jk.matwgt29', 'jk.matwgt30',",
  "    'jk.matwgt31', 'jk.matwgt32', 'jk.matwgt33', 'jk.matwgt34', 'jk.matwgt35', 'jk.matwgt36', 'jk.matwgt37', 'jk.matwgt38', 'jk.matwgt39', 'jk.matwgt40', 'jk.matwgt41', 'jk.matwgt42', 'jk.matwgt43', 'jk.matwgt44', 'jk.matwgt45', 'jk.matwgt46', 'jk.matwgt47', 'jk.matwgt48', 'jk.matwgt49', 'jk.matwgt50', 'jk.matwgt51', 'jk.matwgt52', 'jk.matwgt53', 'jk.matwgt54', 'jk.matwgt55', 'jk.matwgt56', 'jk.matwgt57', 'jk.matwgt58', 'jk.matwgt59',",
  "    'jk.matwgt60', 'jk.matwgt61', 'jk.matwgt62', 'jk.matwgt63', 'jk.matwgt64', 'jk.matwgt65', 'jk.matwgt66', 'jk.matwgt67', 'jk.matwgt68', 'jk.matwgt69', 'jk.matwgt70', 'jk.matwgt71', 'jk.matwgt72', 'jk.matwgt73', 'jk.matwgt74', 'jk.matwgt75', 'jk.matwgt76', 'jk.matwgt77', 'jk.matwgt78', 'jk.matwgt79', 'jk.matwgt80', 'jk.matwgt81', 'jk.matwgt82', 'jk.matwgt83', 'jk.matwgt84', 'jk.matwgt85', 'jk.matwgt86', 'jk.matwgt87', 'jk.matwgt88',",
  "    'jk.matwgt89', 'jk.matwgt90', 'jk.matwgt91', 'jk.matwgt92', 'jk.matwgt93', 'jk.matwgt94', 'jk.matwgt95', 'jk.matwgt96', 'jk.matwgt97', 'jk.matwgt98', 'jk.matwgt99', 'jk.matwgt100', 'jk.matwgt101', 'jk.matwgt102', 'jk.matwgt103', 'jk.matwgt104', 'jk.matwgt105', 'jk.matwgt106', 'jk.matwgt107', 'jk.matwgt108', 'jk.matwgt109', 'jk.matwgt110', 'jk.matwgt111', 'jk.matwgt112', 'jk.matwgt113', 'jk.matwgt114', 'jk.matwgt115', 'jk.matwgt116',",
  "    'jk.matwgt117', 'jk.matwgt118', 'jk.matwgt119', 'jk.matwgt120', 'jk.matwgt121', 'jk.matwgt122', 'jk.matwgt123', 'jk.matwgt124', 'jk.matwgt125', 'jk.matwgt126', 'jk.matwgt127', 'jk.matwgt128', 'jk.matwgt129', 'jk.matwgt130', 'jk.matwgt131', 'jk.matwgt132', 'jk.matwgt133', 'jk.matwgt134', 'jk.matwgt135', 'jk.matwgt136', 'jk.matwgt137', 'jk.matwgt138', 'jk.matwgt139', 'jk.matwgt140', 'jk.matwgt141', 'jk.matwgt142', 'jk.matwgt143',",
  "    'jk.matwgt144', 'jk.matwgt145', 'jk.matwgt146', 'jk.matwgt147', 'jk.matwgt148', 'jk.matwgt149', and 'jk.matwgt150'",
  "",
  "  'sciwgt' with 150 JK replicate weights.", "    Jackknife replicate weight variables associated with the full sample weight 'sciwgt':",
  "    'jk.sciwgt1', 'jk.sciwgt2', 'jk.sciwgt3', 'jk.sciwgt4', 'jk.sciwgt5', 'jk.sciwgt6', 'jk.sciwgt7', 'jk.sciwgt8', 'jk.sciwgt9', 'jk.sciwgt10', 'jk.sciwgt11', 'jk.sciwgt12', 'jk.sciwgt13', 'jk.sciwgt14', 'jk.sciwgt15', 'jk.sciwgt16', 'jk.sciwgt17', 'jk.sciwgt18', 'jk.sciwgt19', 'jk.sciwgt20', 'jk.sciwgt21', 'jk.sciwgt22', 'jk.sciwgt23', 'jk.sciwgt24', 'jk.sciwgt25', 'jk.sciwgt26', 'jk.sciwgt27', 'jk.sciwgt28', 'jk.sciwgt29', 'jk.sciwgt30',",
  "    'jk.sciwgt31', 'jk.sciwgt32', 'jk.sciwgt33', 'jk.sciwgt34', 'jk.sciwgt35', 'jk.sciwgt36', 'jk.sciwgt37', 'jk.sciwgt38', 'jk.sciwgt39', 'jk.sciwgt40', 'jk.sciwgt41', 'jk.sciwgt42', 'jk.sciwgt43', 'jk.sciwgt44', 'jk.sciwgt45', 'jk.sciwgt46', 'jk.sciwgt47', 'jk.sciwgt48', 'jk.sciwgt49', 'jk.sciwgt50', 'jk.sciwgt51', 'jk.sciwgt52', 'jk.sciwgt53', 'jk.sciwgt54', 'jk.sciwgt55', 'jk.sciwgt56', 'jk.sciwgt57', 'jk.sciwgt58', 'jk.sciwgt59',",
  "    'jk.sciwgt60', 'jk.sciwgt61', 'jk.sciwgt62', 'jk.sciwgt63', 'jk.sciwgt64', 'jk.sciwgt65', 'jk.sciwgt66', 'jk.sciwgt67', 'jk.sciwgt68', 'jk.sciwgt69', 'jk.sciwgt70', 'jk.sciwgt71', 'jk.sciwgt72', 'jk.sciwgt73', 'jk.sciwgt74', 'jk.sciwgt75', 'jk.sciwgt76', 'jk.sciwgt77', 'jk.sciwgt78', 'jk.sciwgt79', 'jk.sciwgt80', 'jk.sciwgt81', 'jk.sciwgt82', 'jk.sciwgt83', 'jk.sciwgt84', 'jk.sciwgt85', 'jk.sciwgt86', 'jk.sciwgt87', 'jk.sciwgt88',",
  "    'jk.sciwgt89', 'jk.sciwgt90', 'jk.sciwgt91', 'jk.sciwgt92', 'jk.sciwgt93', 'jk.sciwgt94', 'jk.sciwgt95', 'jk.sciwgt96', 'jk.sciwgt97', 'jk.sciwgt98', 'jk.sciwgt99', 'jk.sciwgt100', 'jk.sciwgt101', 'jk.sciwgt102', 'jk.sciwgt103', 'jk.sciwgt104', 'jk.sciwgt105', 'jk.sciwgt106', 'jk.sciwgt107', 'jk.sciwgt108', 'jk.sciwgt109', 'jk.sciwgt110', 'jk.sciwgt111', 'jk.sciwgt112', 'jk.sciwgt113', 'jk.sciwgt114', 'jk.sciwgt115', 'jk.sciwgt116',",
  "    'jk.sciwgt117', 'jk.sciwgt118', 'jk.sciwgt119', 'jk.sciwgt120', 'jk.sciwgt121', 'jk.sciwgt122', 'jk.sciwgt123', 'jk.sciwgt124', 'jk.sciwgt125', 'jk.sciwgt126', 'jk.sciwgt127', 'jk.sciwgt128', 'jk.sciwgt129', 'jk.sciwgt130', 'jk.sciwgt131', 'jk.sciwgt132', 'jk.sciwgt133', 'jk.sciwgt134', 'jk.sciwgt135', 'jk.sciwgt136', 'jk.sciwgt137', 'jk.sciwgt138', 'jk.sciwgt139', 'jk.sciwgt140', 'jk.sciwgt141', 'jk.sciwgt142', 'jk.sciwgt143',",
  "    'jk.sciwgt144', 'jk.sciwgt145', 'jk.sciwgt146', 'jk.sciwgt147', 'jk.sciwgt148', 'jk.sciwgt149', and 'jk.sciwgt150'",
  ""
)

pctREF <- c(
  "Percentile", "Call: percentile(variable = \"mmat\", percentiles = c(0, 1, 25, 50, 75, ",
  "    99, 100), data = usa4.07, pctMethod = \"unbiased\")", "full data n: 12883",
  "n used: 7896", "", " percentile estimate      se     df confInt.ci_lower confInt.ci_upper",
  "          0   248.67 14.4169  0.000           248.67           248.67",
  "          1   345.74  6.0948 19.316           328.83           357.01",
  "         25   479.13  2.3386 73.969           473.58           484.61",
  "         50   530.75  2.5736 72.190           525.59           535.87",
  "         75   581.48  2.9514 84.268           576.13           586.66",
  "         99   692.29  4.0554 12.058           686.37           702.66",
  "        100   781.63 18.7631  0.000           781.63           781.63"
)

logit1REF <- c(
  "",
  "Formula: readSciOften ~ itsex + at4gsex",
  "Family: binomial (logit)",
  "",
  "Weight variable: 'tchwgt'",
  "Variance method: jackknife",
  "JK replicates: 150",
  "full data n: 12883",
  "n used: 11668",
  "",
  "Coefficients:",
  "                 coef        se         t     dof Pr(>|t|)    ",
  "(Intercept)  0.848121  0.038575 21.986264 148.760   <2e-16 ***",
  "itsexBOY     0.082956  0.051089  1.623764 141.522   0.1067    ",
  "at4gsexMALE  0.031276  0.113342  0.275942  67.495   0.7834    ",
  "---",
  "Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1"
)

probit1REF <- c(
  "",
  "Formula: readSciOften ~ itsex + at4gsex",
  "Family: binomial (probit)",
  "",
  "Weight variable: 'tchwgt'",
  "Variance method: jackknife",
  "JK replicates: 150",
  "full data n: 12883",
  "n used: 11668",
  "",
  "Coefficients:",
  "                 coef        se         t     dof Pr(>|t|)    ",
  "(Intercept)  0.524867  0.023313 22.514233 148.921   <2e-16 ***",
  "itsexBOY     0.049938  0.030725  1.625314 142.079   0.1063    ",
  "at4gsexMALE  0.018996  0.068057  0.279122  67.588   0.7810    ",
  "---",
  "Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1"
)

spv <- c(
  "There are 14 subject scale(s) or subscale(s) in this edsurvey.data.frame:",
  "'mmat' subject scale or subscale with 5 plausible values (the default).",
  "  The plausible value variables are: 'asmmat01', 'asmmat02', 'asmmat03', 'asmmat04', and 'asmmat05'",
  "",
  "'ssci' subject scale or subscale with 5 plausible values.",
  "  The plausible value variables are: 'asssci01', 'asssci02', 'asssci03', 'asssci04', and 'asssci05'",
  "",
  "'mdat' subject scale or subscale with 5 plausible values.",
  "  The plausible value variables are: 'asmdat01', 'asmdat02', 'asmdat03', 'asmdat04', and 'asmdat05'",
  "",
  "'mgeo' subject scale or subscale with 5 plausible values.",
  "  The plausible value variables are: 'asmgeo01', 'asmgeo02', 'asmgeo03', 'asmgeo04', and 'asmgeo05'",
  "",
  "'mnum' subject scale or subscale with 5 plausible values.",
  "  The plausible value variables are: 'asmnum01', 'asmnum02', 'asmnum03', 'asmnum04', and 'asmnum05'",
  "",
  "'sear' subject scale or subscale with 5 plausible values.",
  "  The plausible value variables are: 'assear01', 'assear02', 'assear03', 'assear04', and 'assear05'",
  "",
  "'slif' subject scale or subscale with 5 plausible values.",
  "  The plausible value variables are: 'asslif01', 'asslif02', 'asslif03', 'asslif04', and 'asslif05'",
  "",
  "'sphy' subject scale or subscale with 5 plausible values.",
  "  The plausible value variables are: 'assphy01', 'assphy02', 'assphy03', 'assphy04', and 'assphy05'",
  "",
  "'mkno' subject scale or subscale with 5 plausible values.",
  "  The plausible value variables are: 'asmkno01', 'asmkno02', 'asmkno03', 'asmkno04', and 'asmkno05'",
  "",
  "'mapp' subject scale or subscale with 5 plausible values.",
  "  The plausible value variables are: 'asmapp01', 'asmapp02', 'asmapp03', 'asmapp04', and 'asmapp05'",
  "",
  "'mrea' subject scale or subscale with 5 plausible values.",
  "  The plausible value variables are: 'asmrea01', 'asmrea02', 'asmrea03', 'asmrea04', and 'asmrea05'",
  "",
  "'skno' subject scale or subscale with 5 plausible values.",
  "  The plausible value variables are: 'asskno01', 'asskno02', 'asskno03', 'asskno04', and 'asskno05'",
  "",
  "'sapp' subject scale or subscale with 5 plausible values.",
  "  The plausible value variables are: 'assapp01', 'assapp02', 'assapp03', 'assapp04', and 'assapp05'",
  "",
  "'srea' subject scale or subscale with 5 plausible values.",
  "  The plausible value variables are: 'assrea01', 'assrea02', 'assrea03', 'assrea04', and 'assrea05'",
  ""
)

assignTest1 <- c(
  "    asmrea01        asmrea02        asmrea03        asmrea04        asmrea05    ",
  " Min.   :  5.0   Min.   :  5.0   Min.   :  5.0   Min.   :  5.0   Min.   :  5.0  ",
  " 1st Qu.:246.5   1st Qu.:246.8   1st Qu.:249.8   1st Qu.:248.5   1st Qu.:247.3  ",
  " Median :327.7   Median :326.9   Median :328.6   Median :328.6   Median :329.4  ",
  " Mean   :328.0   Mean   :328.4   Mean   :329.9   Mean   :329.1   Mean   :328.8  ",
  " 3rd Qu.:410.8   3rd Qu.:412.3   3rd Qu.:412.5   3rd Qu.:410.5   3rd Qu.:413.4  ",
  " Max.   :784.2   Max.   :732.8   Max.   :729.6   Max.   :746.9   Max.   :707.6  "
)

mmlIntREF <- c(
  "  (Intercept) Population SD ",
  "       539.82         81.72 "
)


mmlSumREF <- c(
  "Call:",
  "mml.sdf(formula = mmat ~ 1, data = usa4.15, weightVar = \"totwgt\", ",
  "    verbose = TRUE)",
  "Summary Call:",
  "summary.mml.sdf(object = mml1)",
  "",
  "Summary:",
  "            Estimate StdErr t.value  dof Pr(>|t|)    ",
  "(Intercept)   539.82   2.22  243.15 54.2   <2e-16 ***",
  "---",
  "Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1",
  "",
  "Residual Variance Estimate:",
  "              Estimate StdErr",
  "Population SD    81.72  1.422",
  "",
  "Convergence = converged",
  "Iterations = 12",
  "LogLike = -52453394.92",
  "Observations = 10017",
  "Weighted observations = 3752699.01"
)

