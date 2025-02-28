# lm.sdf returns

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["call", "formula", "coef", "se", "Vimp", "Vjrr", "M", "varm", "coefm", "coefmat", "r.squared", "weight", "npv", "jrrIMax", "njk", "varMethod", "residuals", "fitted.values", "residual.df", "PV.residuals", "PV.fitted.values", "B", "U", "rbar", "Ttilde", "waldDenomBaseDof", "n0", "nUsed", "Xstdev", "varSummary"]
        }
      },
      "value": [
        {
          "type": "character",
          "attributes": {},
          "value": ["lm.sdf", "composite ~ dsex + b017451", "sdf", "Taylor", "FALSE"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["~", "composite", "dsex + b017451"]
        },
        {
          "type": "double",
          "attributes": {
            "names": {
              "type": "character",
              "attributes": {},
              "value": ["(Intercept)", "dsexFemale", "b017451Once every few weeks", "b017451About once a week", "b0174512 or 3 times a week", "b017451Every day"]
            }
          },
          "value": [270.41112096, -2.95857831, 4.23341438, 11.22612315, 14.94590851, 7.5299837]
        },
        {
          "type": "double",
          "attributes": {
            "names": {
              "type": "character",
              "attributes": {},
              "value": ["(Intercept)", "dsexFemale", "b017451Once every few weeks", "b017451About once a week", "b0174512 or 3 times a week", "b017451Every day"]
            }
          },
          "value": [1.04063702, 0.64489347, 1.19561969, 1.2861361, 1.18293533, 1.32429189]
        },
        {
          "type": "double",
          "attributes": {
            "names": {
              "type": "character",
              "attributes": {},
              "value": ["(Intercept)", "dsexFemale", "b017451Once every few weeks", "b017451About once a week", "b0174512 or 3 times a week", "b017451Every day"]
            }
          },
          "value": [0.03171231, 0.01180441, 0.11588306, 0.24994947, 0.06214487, 0.21697236]
        },
        {
          "type": "double",
          "attributes": {
            "names": {
              "type": "character",
              "attributes": {},
              "value": ["(Intercept)", "dsexFemale", "b017451Once every few weeks", "b017451About once a week", "b0174512 or 3 times a week", "b017451Every day"]
            }
          },
          "value": [1.05121309, 0.40408317, 1.31362337, 1.40419661, 1.33719112, 1.53677665]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [5]
        },
        {
          "type": "double",
          "attributes": {
            "dim": {
              "type": "integer",
              "attributes": {},
              "value": [5, 6]
            }
          },
          "value": [1.01870531, 1.06539345, 1.00533367, 1.03818196, 1.12845106, 0.35323443, 0.40526154, 0.38738681, 0.45317031, 0.42136277, 1.28554255, 1.32326628, 1.22584955, 1.26831315, 1.46514534, 1.33816859, 1.27099897, 1.36332917, 1.61859515, 1.42989116, 1.34394045, 1.37581298, 1.25205841, 1.39799376, 1.31615001, 1.50264549, 1.41008769, 1.63010819, 1.64394241, 1.49709947]
        },
        {
          "type": "double",
          "attributes": {
            "dim": {
              "type": "integer",
              "attributes": {},
              "value": [5, 6]
            }
          },
          "value": [270.29902611, 270.36985477, 270.35897829, 270.6977271, 270.33001852, -2.93888692, -3.03862501, -2.82749376, -3.07415624, -2.91372962, 4.38903707, 4.32933571, 3.889315, 3.94045031, 4.61893381, 11.44675308, 11.2824696, 11.59629029, 10.43624069, 11.3688621, 15.31232068, 14.97664155, 14.89902307, 14.7059405, 14.83561674, 7.99325876, 7.40171989, 7.73429696, 6.86981763, 7.65082525]
        },
        {
          "type": "list",
          "attributes": {
            "names": {
              "type": "character",
              "attributes": {},
              "value": ["coef", "se", "t", "dof", "Pr(>|t|)"]
            },
            "row.names": {
              "type": "character",
              "attributes": {},
              "value": ["(Intercept)", "dsexFemale", "b017451Once every few weeks", "b017451About once a week", "b0174512 or 3 times a week", "b017451Every day"]
            },
            "class": {
              "type": "character",
              "attributes": {},
              "value": ["data.frame"]
            }
          },
          "value": [
            {
              "type": "double",
              "attributes": {},
              "value": [270.41112096, -2.95857831, 4.23341438, 11.22612315, 14.94590851, 7.5299837]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [1.04063702, 0.64489347, 1.19561969, 1.2861361, 1.18293533, 1.32429189]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [259.85153038, -4.58770083, 3.54077005, 8.72856544, 12.63459477, 5.68604531]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [44.00721139, 65.672089, 61.97535145, 51.78741346, 62.56802681, 50.19726377]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [0, 0.00002072, 0.00076375, 9.46975831e-12, 0, 6.62896132e-07]
            }
          ]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [0.02238869]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["origwt"]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [5]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [5]
        },
        {
          "type": "logical",
          "attributes": {},
          "value": [null]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["Taylor series"]
        },
        {
          "type": "double",
          "attributes": {
            "dim": {
              "type": "integer",
              "attributes": {},
              "value": [6, 5]
            }
          },
          "value": [40.06889534, 9.7513342, 67.73747365, 70.81889534, 6.75404297, 42.59297053, 25.73889534, 5.2513342, 63.04747365, 43.84889534, -26.60595703, 53.23297053, 18.66889534, 1.7713342, 54.49747365, 49.92889534, -8.68595703, 42.71297053, 51.02889534, 11.3513342, 77.47747365, 55.40889534, 5.81404297, 48.71297053, 37.75889534, 7.5513342, 67.27747365, 49.37889534, 13.35404297, 34.66297053]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [277.94110466, 278.6786658, 274.98252635, 277.94110466, 271.68595703, 285.35702947]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [16325]
        },
        {
          "type": "double",
          "attributes": {
            "dim": {
              "type": "integer",
              "attributes": {},
              "value": [6, 5]
            }
          },
          "value": [39.71771513, 9.62310773, 67.36660205, 70.46771513, 6.69082373, 42.33865321, 25.90842534, 5.31630063, 63.29705034, 44.01842534, -26.58056547, 53.24350368, 18.51672475, 1.32222518, 54.21421851, 49.77672475, -8.42079953, 42.81199864, 51.40245527, 11.97018845, 77.96661151, 55.78245527, 5.93597883, 48.6663324, 37.71915623, 7.444849, 67.19288585, 49.33915623, 13.0047773, 34.85436474]
        },
        {
          "type": "double",
          "attributes": {
            "dim": {
              "type": "integer",
              "attributes": {},
              "value": [6, 5]
            }
          },
          "value": [278.29228487, 278.80689227, 275.35339795, 278.29228487, 271.74917627, 285.61134679, 277.77157466, 278.61369937, 274.73294966, 277.77157466, 271.66056547, 285.34649632, 278.09327525, 279.12777482, 275.26578149, 278.09327525, 271.42079953, 285.25800136, 277.56754473, 278.05981155, 274.49338849, 277.56754473, 271.56402117, 285.4036676, 277.98084377, 278.785151, 275.06711415, 277.98084377, 272.0352227, 285.16563526]
        },
        {
          "type": "double",
          "attributes": {
            "dim": {
              "type": "integer",
              "attributes": {},
              "value": [6, 6]
            }
          },
          "value": [0.02642692, -0.01062547, -0.0296731, -0.07107996, -0.02493195, -0.06407486, -0.01062547, 0.00983701, 0.00035759, 0.036513, 0.00534942, 0.03197304, -0.0296731, 0.00035759, 0.09656922, 0.04970035, 0.02597141, 0.05737013, -0.07107996, 0.036513, 0.04970035, 0.20829123, 0.05975533, 0.1773294, -0.02493195, 0.00534942, 0.02597141, 0.05975533, 0.05178739, 0.07532982, -0.06407486, 0.03197304, 0.05737013, 0.1773294, 0.07532982, 0.1808103]
        },
        {
          "type": "double",
          "attributes": {
            "dim": {
              "type": "integer",
              "attributes": {},
              "value": [6, 6]
            },
            "dimnames": {
              "type": "list",
              "attributes": {},
              "value": [
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["(Intercept)", "dsexFemale", "b017451Once every few weeks", "b017451About once a week", "b0174512 or 3 times a week", "b017451Every day"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["(Intercept)", "dsexFemale", "b017451Once every few weeks", "b017451About once a week", "b0174512 or 3 times a week", "b017451Every day"]
                }
              ]
            }
          },
          "value": [1.05121309, -0.07029324, -0.66440909, -0.47033386, -0.41107606, -0.55378954, -0.07029324, 0.40408317, -0.17479697, -0.11442681, -0.10989539, -0.1622518, -0.66440909, -0.17479697, 1.31362337, 0.7614072, 0.58265318, 0.56678918, -0.47033386, -0.11442681, 0.7614072, 1.40419661, 0.69930659, 0.72512739, -0.41107606, -0.10989539, 0.58265318, 0.69930659, 1.33719112, 0.42862988, -0.55378954, -0.1622518, 0.56678918, 0.72512739, 0.42862988, 1.53677665]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [0.0834612]
        },
        {
          "type": "double",
          "attributes": {
            "dim": {
              "type": "integer",
              "attributes": {},
              "value": [6, 6]
            },
            "dimnames": {
              "type": "list",
              "attributes": {},
              "value": [
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["(Intercept)", "dsexFemale", "b017451Once every few weeks", "b017451About once a week", "b0174512 or 3 times a week", "b017451Every day"]
                },
                {
                  "type": "character",
                  "attributes": {},
                  "value": ["(Intercept)", "dsexFemale", "b017451Once every few weeks", "b017451About once a week", "b0174512 or 3 times a week", "b017451Every day"]
                }
              ]
            }
          },
          "value": [1.1389486, -0.07616, -0.71986147, -0.50958849, -0.44538496, -0.60000948, -0.07616, 0.43780844, -0.18938573, -0.12397701, -0.11906739, -0.17579353, -0.71986147, -0.18938573, 1.42325996, 0.82495516, 0.63128212, 0.61409408, -0.50958849, -0.12397701, 0.82495516, 1.52139255, 0.75767156, 0.7856474, -0.44538496, -0.11906739, 0.63128212, 0.75767156, 1.4487947, 0.46440385, -0.60000948, -0.17579353, 0.61409408, 0.7856474, 0.46440385, 1.66503788]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [63]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [17606]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [16331]
        },
        {
          "type": "list",
          "attributes": {
            "names": {
              "type": "character",
              "attributes": {},
              "value": ["outcome.std", "(Intercept)", "dsexFemale", "b017451Once every few weeks", "b017451About once a week", "b0174512 or 3 times a week"]
            }
          },
          "value": [
            {
              "type": "double",
              "attributes": {},
              "value": [36.34507488]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [0]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [0.50001255]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [0.39288096]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [0.38048056]
            },
            {
              "type": "double",
              "attributes": {},
              "value": [0.40354302]
            }
          ]
        },
        {
          "type": "list",
          "attributes": {},
          "value": [
            {
              "type": "list",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["summary"]
                },
                "class": {
                  "type": "character",
                  "attributes": {},
                  "value": ["summary2"]
                }
              },
              "value": [
                {
                  "type": "list",
                  "attributes": {
                    "names": {
                      "type": "character",
                      "attributes": {},
                      "value": ["Variable", "N", "Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.", "SD", "NA's"]
                    },
                    "class": {
                      "type": "character",
                      "attributes": {},
                      "value": ["data.frame"]
                    },
                    "row.names": {
                      "type": "integer",
                      "attributes": {},
                      "value": [1, 2, 3, 4, 5]
                    }
                  },
                  "value": [
                    {
                      "type": "character",
                      "attributes": {},
                      "value": ["mrpcm1", "mrpcm2", "mrpcm3", "mrpcm4", "mrpcm5"]
                    },
                    {
                      "type": "double",
                      "attributes": {},
                      "value": [16331, 16331, 16331, 16331, 16331]
                    },
                    {
                      "type": "double",
                      "attributes": {},
                      "value": [130.53, 124.16, 115.09, 137.19, 123.58]
                    },
                    {
                      "type": "double",
                      "attributes": {},
                      "value": [252.45166667, 252.69166667, 252.37166667, 252.815, 252.86]
                    },
                    {
                      "type": "double",
                      "attributes": {},
                      "value": [277.5, 277.5, 277.47, 277.64, 277.33]
                    },
                    {
                      "type": "double",
                      "attributes": {},
                      "value": [276.13928051, 275.92952544, 275.93477803, 276.0169365, 275.9802878]
                    },
                    {
                      "type": "double",
                      "attributes": {},
                      "value": [300.79833333, 300.76, 300.67833333, 300.82833333, 300.61]
                    },
                    {
                      "type": "double",
                      "attributes": {},
                      "value": [410.8, 408.58, 398.17, 407.41, 395.96]
                    },
                    {
                      "type": "double",
                      "attributes": {},
                      "value": [35.69408638, 35.85086763, 35.90873088, 35.72197981, 35.88011548]
                    },
                    {
                      "type": "double",
                      "attributes": {},
                      "value": [0, 0, 0, 0, 0]
                    }
                  ]
                }
              ]
            },
            {
              "type": "list",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["summary"]
                },
                "class": {
                  "type": "character",
                  "attributes": {},
                  "value": ["summary2"]
                }
              },
              "value": [
                {
                  "type": "list",
                  "attributes": {
                    "names": {
                      "type": "character",
                      "attributes": {},
                      "value": ["dsex", "N", "Percent"]
                    },
                    "row.names": {
                      "type": "integer",
                      "attributes": {},
                      "value": [1, 2]
                    },
                    "class": {
                      "type": "character",
                      "attributes": {},
                      "value": ["data.frame"]
                    }
                  },
                  "value": [
                    {
                      "type": "integer",
                      "attributes": {
                        "levels": {
                          "type": "character",
                          "attributes": {},
                          "value": ["Male", "Female"]
                        },
                        "class": {
                          "type": "character",
                          "attributes": {},
                          "value": ["factor"]
                        }
                      },
                      "value": [1, 2]
                    },
                    {
                      "type": "integer",
                      "attributes": {},
                      "value": [8163, 8168]
                    },
                    {
                      "type": "double",
                      "attributes": {},
                      "value": [49.98469169, 50.01530831]
                    }
                  ]
                }
              ]
            },
            {
              "type": "list",
              "attributes": {
                "names": {
                  "type": "character",
                  "attributes": {},
                  "value": ["summary"]
                },
                "class": {
                  "type": "character",
                  "attributes": {},
                  "value": ["summary2"]
                }
              },
              "value": [
                {
                  "type": "list",
                  "attributes": {
                    "names": {
                      "type": "character",
                      "attributes": {},
                      "value": ["b017451", "N", "Percent"]
                    },
                    "row.names": {
                      "type": "integer",
                      "attributes": {},
                      "value": [1, 2, 3, 4, 5]
                    },
                    "class": {
                      "type": "character",
                      "attributes": {},
                      "value": ["data.frame"]
                    }
                  },
                  "value": [
                    {
                      "type": "integer",
                      "attributes": {
                        "levels": {
                          "type": "character",
                          "attributes": {},
                          "value": ["Never or hardly ever", "Once every few weeks", "About once a week", "2 or 3 times a week", "Every day"]
                        },
                        "class": {
                          "type": "character",
                          "attributes": {},
                          "value": ["factor"]
                        }
                      },
                      "value": [1, 2, 3, 4, 5]
                    },
                    {
                      "type": "integer",
                      "attributes": {},
                      "value": [3837, 3147, 2853, 3362, 3132]
                    },
                    {
                      "type": "double",
                      "attributes": {},
                      "value": [23.49519319, 19.27009981, 17.46984263, 20.58661441, 19.17824995]
                    }
                  ]
                }
              ]
            }
          ]
        }
      ]
    }

