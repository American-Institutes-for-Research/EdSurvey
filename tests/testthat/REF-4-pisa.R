pisaedTable1REF <- c(
  "",
  "Formula: math ~ st04q01 + st20q01 ",
  "",
  "Plausible values: 5",
  "jrrIMax: 1",
  "Weight variable: 'w_fstuwt'",
  "Variance method: jackknife",
  "JK replicates: 80",
  "full data n: 4978",
  "n used: 4873",
  "",
  "",
  "Summary Table:",
  " st04q01         st20q01    N     WTD_N       PCT   SE(PCT)     MEAN  SE(MEAN)",
  "  Female Country of test 2234 1575416.5 92.425753 0.8038984 481.1794  4.019205",
  "  Female   Other country  180  129104.6  7.574247 0.8038984 463.6272 10.355889",
  "    Male Country of test 2272 1611412.6 91.796017 0.9558501 486.8819  3.853613",
  "    Male   Other country  187  144015.0  8.203983 0.9558501 474.1468  9.474347"
)

plm1REF <- c(
  "             (Intercept)             st29q06Agree          st29q06Disagree st29q06Strongly disagree           sc01q01Private ",
  "              506.993125               -21.828757               -32.381549               -52.944871                 2.408131 "
)

pgap1REF <- c(
  "Call: gap(variable = \"math\", data = usaINT2012, groupA = st04q01 == ",
  "    \"Male\", groupB = st04q01 == \"Female\", weightVar = \"w_fstuwt\")",
  "",
  "Labels:",
  " group          definition nFullData nUsed",
  "     A   st04q01 == \"Male\"      4978  2525",
  "     B st04q01 == \"Female\"      4978  2453",
  "",
  "Percentage:",
  "     pctA    pctAse     pctB    pctBse   diffAB      covAB diffABse diffABpValue    dofAB",
  " 50.98087 0.7182871 49.01913 0.7182871 1.961734 -0.5159363 1.436574     0.174861 110.0328",
  "",
  "Results:",
  " estimateA estimateAse estimateB estimateBse   diffAB    covAB diffABse diffABpValue    dofAB",
  "   483.647    3.800262  478.9953     3.92109 4.651662 11.01904 2.789059   0.09911296 83.14098"
)

al1REF <- c(
  "",
  "AchievementVars: cpro",
  "aggregateBy: st04q01",
  "",
  "Achievement Level Cutpoints:",
  "358.49 423.42 488.35 553.28 618.21 683.14 ",
  "",
  "Plausible values: 5",
  "jrrIMax: 1",
  "Weight variable: 'w_fstuwt'",
  "Variance method: jackknife",
  "JK replicates: 80",
  "full data n: 5177",
  "n used: 5177",
  "",
  "",
  "Discrete",
  "                cpro_Level st04q01     N      wtdN   Percent StandardError",
  " Below Proficiency Level 1  Female  95.4  1541.697  3.539350     0.7086689",
  "    At Proficiency Level 1  Female 234.2  3815.988  8.760555     0.8224039",
  "    At Proficiency Level 2  Female 524.6  8669.991 19.904132     0.9013387",
  "    At Proficiency Level 3  Female 771.0 12722.017 29.206570     1.3662494",
  "    At Proficiency Level 4  Female 644.6 10711.838 24.591702     1.3001839",
  "    At Proficiency Level 5  Female 300.8  4985.134 11.444621     1.1955623",
  "    At Proficiency Level 6  Female  66.4  1112.086  2.553071     0.5394964",
  " Below Proficiency Level 1    Male  68.8  1083.360  2.616423     0.5358135",
  "    At Proficiency Level 1    Male 167.6  2578.450  6.227218     0.6980039",
  "    At Proficiency Level 2    Male 383.8  6222.924 15.028992     1.1775214",
  "    At Proficiency Level 3    Male 648.8 10591.764 25.580183     1.3363213",
  "    At Proficiency Level 4    Male 696.6 11515.983 27.812266     1.7109083",
  "    At Proficiency Level 5    Male 430.6  7036.873 16.994760     1.1910320",
  "    At Proficiency Level 6    Male 143.8  2376.777  5.740157     0.6795269"
)

pgap2REF <- c(
  "gapList",
  "Call: gap(variable = \"math\", data = usaINT2012, groupA = st04q01 == ",
  "    \"Male\", groupB = st04q01 == \"Female\", percentiles = c(50, ",
  "    90), weightVar = \"w_fstuwt\", pctMethod = \"symmetric\")",
  "",
  "Labels:",
  " group          definition",
  "     A   st04q01 == \"Male\"",
  "     B st04q01 == \"Female\"",
  "",
  "Percentage:",
  "     pctA    pctAse     pctB    pctBse   diffAB      covAB diffABse diffABpValue    dofAB",
  " 50.98087 0.7182871 49.01913 0.7182871 1.961734 -0.5159363 1.436574     0.174861 110.0328",
  "",
  "Results:",
  " percentiles estimateA estimateAse estimateB estimateBse   diffAB     covAB diffABse diffABpValue    dofAB",
  "          50  480.7124    4.075956  474.6266    4.498829 6.085824 11.494158 3.723515    0.1064350 73.66955",
  "          90  605.1735    3.942558  595.3941    8.603019 9.779404  7.893203 8.588906    0.2579161 89.16957"
)

pvREF <- c(
  "There are 10 subject scale(s) or subscale(s) in this edsurvey.data.frame:",
  "'math' subject scale or subscale with 5 plausible values (the default).",
  "  The plausible value variables are: 'pv1math', 'pv2math', 'pv3math', 'pv4math', and 'pv5math'",
  "",
  "'macc' subject scale or subscale with 5 plausible values.",
  "  The plausible value variables are: 'pv1macc', 'pv2macc', 'pv3macc', 'pv4macc', and 'pv5macc'",
  "",
  "'macq' subject scale or subscale with 5 plausible values.",
  "  The plausible value variables are: 'pv1macq', 'pv2macq', 'pv3macq', 'pv4macq', and 'pv5macq'",
  "",
  "'macs' subject scale or subscale with 5 plausible values.",
  "  The plausible value variables are: 'pv1macs', 'pv2macs', 'pv3macs', 'pv4macs', and 'pv5macs'",
  "",
  "'macu' subject scale or subscale with 5 plausible values.",
  "  The plausible value variables are: 'pv1macu', 'pv2macu', 'pv3macu', 'pv4macu', and 'pv5macu'",
  "",
  "'mape' subject scale or subscale with 5 plausible values.",
  "  The plausible value variables are: 'pv1mape', 'pv2mape', 'pv3mape', 'pv4mape', and 'pv5mape'",
  "",
  "'mapf' subject scale or subscale with 5 plausible values.",
  "  The plausible value variables are: 'pv1mapf', 'pv2mapf', 'pv3mapf', 'pv4mapf', and 'pv5mapf'",
  "",
  "'mapi' subject scale or subscale with 5 plausible values.",
  "  The plausible value variables are: 'pv1mapi', 'pv2mapi', 'pv3mapi', 'pv4mapi', and 'pv5mapi'",
  "",
  "'read' subject scale or subscale with 5 plausible values.",
  "  The plausible value variables are: 'pv1read', 'pv2read', 'pv3read', 'pv4read', and 'pv5read'",
  "",
  "'scie' subject scale or subscale with 5 plausible values.",
  "  The plausible value variables are: 'pv1scie', 'pv2scie', 'pv3scie', 'pv4scie', and 'pv5scie'",
  ""
)

swREF <- c(
  "There is 1 full sample weight in this edsurvey.data.frame:",
  "  'w_fstuwt' with 80 JK replicate weights (the default).",
  "    Jackknife replicate weight variables associated with the full sample weight 'w_fstuwt':",
  "    'w_fstr1', 'w_fstr2', 'w_fstr3', 'w_fstr4', 'w_fstr5', 'w_fstr6', 'w_fstr7', 'w_fstr8', 'w_fstr9', 'w_fstr10', 'w_fstr11', 'w_fstr12', 'w_fstr13', 'w_fstr14', 'w_fstr15', 'w_fstr16', 'w_fstr17', 'w_fstr18', 'w_fstr19', 'w_fstr20', 'w_fstr21', 'w_fstr22', 'w_fstr23', 'w_fstr24', 'w_fstr25', 'w_fstr26', 'w_fstr27', 'w_fstr28', 'w_fstr29', 'w_fstr30', 'w_fstr31', 'w_fstr32', 'w_fstr33', 'w_fstr34', 'w_fstr35', 'w_fstr36', 'w_fstr37',",
  "    'w_fstr38', 'w_fstr39', 'w_fstr40', 'w_fstr41', 'w_fstr42', 'w_fstr43', 'w_fstr44', 'w_fstr45', 'w_fstr46', 'w_fstr47', 'w_fstr48', 'w_fstr49', 'w_fstr50', 'w_fstr51', 'w_fstr52', 'w_fstr53', 'w_fstr54', 'w_fstr55', 'w_fstr56', 'w_fstr57', 'w_fstr58', 'w_fstr59', 'w_fstr60', 'w_fstr61', 'w_fstr62', 'w_fstr63', 'w_fstr64', 'w_fstr65', 'w_fstr66', 'w_fstr67', 'w_fstr68', 'w_fstr69', 'w_fstr70', 'w_fstr71', 'w_fstr72', 'w_fstr73', 'w_fstr74',",
  "    'w_fstr75', 'w_fstr76', 'w_fstr77', 'w_fstr78', 'w_fstr79', and 'w_fstr80'",
  ""
)

scREF <- c(
  "Achievement Levels:",
  "  Mathematics:  357.77, 420.07, 482.38, 544.68, 606.99, 669.3",
  "  Problem Solving:  358.49, 423.42, 488.35, 553.28, 618.21, 683.14",
  "  Reading:  262.04, 334.75, 407.47, 480.18, 552.89, 625.61, 698.32"
)
