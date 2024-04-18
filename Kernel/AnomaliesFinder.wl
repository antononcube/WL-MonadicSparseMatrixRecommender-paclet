(* :Title: MonadicAnomaliesFinder *)
(* :Context: MonadicAnomaliesFinder` *)
(* :Author: Anton Antonov *)
(* :Date: 2019-09-13 *)
(* Created with the Wolfram Language Plugin for IntelliJ, see http://wlplugin.halirutan.de/ . *)

(* :Package Version: 0.5 *)
(* :Mathematica Version: 12.0 *)
(* :Copyright: (c) 2019 Anton Antonov *)
(* :Keywords: Anomalies, Outliers *)
(* :Discussion: *)


(**************************************************************)
(* Package definition                                         *)
(**************************************************************)


BeginPackage["AntonAntonov`SparseMatrixRecommender`AnomaliesFinder`"];

Begin["`Private`"];

Needs["AntonAntonov`MonadicSparseMatrixRecommender`"];
Needs["AntonAntonov`OutlierIdentifiers`"];
Needs["AntonAntonov`SSparseMatrix`"];

(**************************************************************)
(* Find anomalies by nearest neighbors                        *)
(**************************************************************)

Clear[SMRMonFindAnomalies];

SyntaxInformation[SMRMonFindAnomalies] = { "ArgumentsPattern" -> { _., OptionsPattern[] } };

Options[SMRMonFindAnomalies] = {
  "NumberOfNearestNeighbors" -> 10,
  "ThresholdsIdentifier" -> (BottomOutliers @* SPLUSQuartileIdentifierParameters),
  "AggregationFunction" -> Mean,
  "Property" -> "SSparseMatrix"
};

SMRMonFindAnomalies[$SMRMonFailure] := $SMRMonFailure;

SMRMonFindAnomalies[xs_, context_Association] := SMRMonFindAnomalies[][xs, context];

SMRMonFindAnomalies[ opts : OptionsPattern[] ][xs_, context_Association] :=
    SMRMonFindAnomalies[ Automatic, opts ][xs, context];

SMRMonFindAnomalies[ arg_, opts : OptionsPattern[] ][xs_, context_Association] :=
    Block[{ nns, outThresholdsFunc, aggrFunc, newContext, prop, smat, recs, outlierInds, outlierThresholds, knownProperties },

      (* Preliminary assignments. *)

      knownProperties = { "Distances", "SSparseMatrix", "RowNames", "Indices", "OutlierThresholds", "Properties" };

      nns = OptionValue[ SMRMonFindAnomalies, "NumberOfNearestNeighbors" ];
      outThresholdsFunc = OptionValue[ SMRMonFindAnomalies, "ThresholdsIdentifier" ];
      aggrFunc = OptionValue[ SMRMonFindAnomalies, "AggregationFunction" ];
      prop = OptionValue[ SMRMonFindAnomalies, "Property" ];

      If[ ! (IntegerQ[nns] && nns > 0),
        Echo[
          "The value of the options \"NumberOfNearestNeighbors\" is expected to be a positive integer.",
          "SMRMonFindAnomalies:"
        ];
        Return[$SMRMonFailure]
      ];

      If[ !KeyExistsQ[context, "M"],
        Echo["Cannot find a recommendation matrix.", "SMRMonFindAnomalies:"];
        Return[$SMRMonFailure]
      ];

      (* If arg is a standard matrix make it an SSparseMatrix. *)
      smat = arg;
      If[ MatrixQ[arg, NumberQ],
        smat = ToSSparseMatrix[ SparseArray[arg], "RowNames" -> Map[ToString, Range[Length[arg]]] ]
      ];

      (* Compute the Cosine/Dot distances to the rows of monad's matrix. *)
      (* If arg/smat is Automatic use monad's matrix. *)
      Which[
        smat === Automatic,
        recs =
            Association[
              Map[
                # ->
                    Fold[
                      SMRMonBind,
                      SMRMonUnit[xs, context],
                      {
                        SMRMonRecommendByHistory[#, nns, "Normalize" -> False],
                        SMRMonTakeValue
                      } ]&,
                RowNames[context["M"]]
              ]
            ],

        SSparseMatrixQ[smat] && ColumnsCount[smat] == ColumnsCount[context["M"]],
        nns = Lookup[ context, "numberOfNNs" ];
        recs =
            Association[
              Map[
                # ->
                    Fold[
                      SMRMonBind,
                      SMRMonUnit[xs, context],
                      {
                        SMRMonRecommendByProfile[ SparseArray[smat[[#, All]]], nns, "Normalize" -> False],
                        SMRMonTakeValue
                      } ]&,
                RowNames[smat]
              ]
            ],

        True,
        Echo[
          "The first argument is expected to be Automatic, a matrix, or SSparseMatrix object.",
          "SMRMonFindAnomalies:"
        ];
        Return[$SMRMonFailure]
      ];

      (* If smat is not Automatic and the outliers thresholds are already in the context
         then this means with have "trained" already. *)
      If[ SSparseMatrixQ[smat] && KeyExistsQ[context, "outlierThresholds"],
        aggrFunc = context["aggregationFunction"]
      ];

      (* Aggregate *)
      recs = Map[ aggrFunc@*Values, recs ];
      (* Probably not a good hack, but for now produces expected results. *)
      (* Takes care of Mean[{}] and similar cases. *)
      recs = If[ !NumericQ[#], 0., #]& /@ recs;

      (* If smat is not Automatic and the outliers thresholds are already in the context then use them. *)
      (* If smat is Automatic then (re-)compute the outlier thresholds. *)
      (* Is storing of the outlier thresholds needed? *)
      (* They seems cheap to compute, and it is confusing to keep as "state" variable. *)
      (*If[ SSparseMatrixQ[smat] && KeyExistsQ[context, "outlierThresholds"],

        aggrFunc = context["aggregationFunction"];
        outlierThresholds = context["outlierThresholds"];
        With[ {th = outlierThresholds},
          outlierInds = OutlierPosition[ Values[recs], th& ]
        ],
        *)(* ELSE *)(*

        outlierThresholds = outThresholdsFunc[ Values[recs] ];
        outlierInds = OutlierPosition[ Values[recs], outThresholdsFunc ]
       ];*)

      outlierThresholds = outThresholdsFunc[ Values[recs] ];
      outlierInds = OutlierPosition[ Values[recs], outThresholdsFunc ];

      (* The updated context. *)
      newContext = Join[ context, <| "outlierThresholds" -> outlierThresholds, "aggregationFunction" -> aggrFunc, "numberOfNNs" -> nns |>];

      (* Return result according to the "Property" option. *)
      Which[
        Length[outlierInds] == 0,
        SMRMonUnit[{}, newContext],

        MemberQ[ ToLowerCase[{ "Distances", "AggregatedDistances" }], ToLowerCase[prop] ],
        SMRMonUnit[ recs, newContext ],

        MemberQ[ ToLowerCase[{ "Indices", "Indexes" }], ToLowerCase[prop] ],
        SMRMonUnit[ outlierInds, newContext ],

        MemberQ[ ToLowerCase[{ "RowNames" }], ToLowerCase[prop] ],
        SMRMonUnit[ RowNames[context["M"]][[outlierInds]], newContext ],

        MemberQ[ ToLowerCase[{ "Thresholds", "OutlierThresholds" }], ToLowerCase[prop] ],
        SMRMonUnit[ outlierThresholds, newContext ],

        ToLowerCase["Properties"] == ToLowerCase[prop],
        Echo[ knownProperties, "SMRMonFindAnomalies:"];
        SMRMonUnit[ knownProperties, newContext ],

        ToLowerCase["SSparseMatrix"] == ToLowerCase[prop],
        SMRMonUnit[ context["M"][[ outlierInds, All ]], newContext ],

        True,
        Echo[ "Unknown property. The property should be one of : " <> ToString[knownProperties] <> ".", "SMRMonFindAnomalies:"];
        $SMRMonFailure
      ]
    ];

SMRMonFindAnomalies[___][xs_, context_Association] :=
    Block[{},
      Echo[
        "The expected signature is SMRMonFindAnomalies[ opts:OptionsPattern[] ].",
        "SMRMonFindAnomalies:"
      ];
      $SMRMonFailure
    ];

End[]; (* `Private` *)

EndPackage[]