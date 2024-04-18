(* :Title: MonadicSparseMatrixRecommender *)
(* :Context: MonadicSparseMatrixRecommender` *)
(* :Author: Anton Antonov *)
(* :Date: 2018-09-02 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: 11.3 *)
(* :Copyright: (c) 2018 antonov *)
(* :Keywords: *)
(* :Discussion:
*
*   # In brief
*
*   This package provides monadic implementation of a Item-Item Recommender (IIR) system.
*   The IIR system is aimed at being fast and agile; see [1,2].
*
*
*   # Discussion on the approach*
*   ...
*
*   # Usage example
*   ...
*
*   # Design considerations
*
*   The monad commands are given "verb-like" names and appropriate synonyms are considered/added.
*
*   It is an interesting question how to create the recommender.
*   If we allow empty IIR monad structure creation SMRMonUnit[] then we have to be able
*   to ingest matrices and datasets for the creation population of the IIR data structures.
*
*
*   # Implementation considerations
*
*   Note that the Sparse Matrix Recommender can be mapped into the Stream Blending Recommender. See [2].
*
*   The easiest way to implement IIR is to reuse the Object-Oriented Programming (OOP) implementation [3].
*   But OOP implementation [3] was developed before the introduction Association and the implementation of the
*   packages SSparseMatrix.m, [4,5], and CrossTabulate.m, [6].
*
*
*   # References
*
*   [1] Anton Antonov, A fast and agile Item-Item Recommender system, (2011), Wolfram Technology Conference.
*
*   [2] Anton Antonov, Mapping Sparse Matrix Recommender to Streams Blending Recommender, (2017), MathematicaForPrediction at GitHub.
*
*   [3] Anton Antonov, Sparse Matrix Recommender Framework Mathematica package, (2014), MathematicaForPrediction at GitHub.
*
*   [4] Anton Antonov, SSparseMatrix Mathematica unit tests, (2018), MathematicaForPrediction at GitHub.
*       URL: https://github.com/antononcube/MathematicaForPrediction/blob/master/UnitTests/SSparseMatrix-tests.wlt
*
*   [5] Anton Antonov, RSparseMatrix Mathematica package, (2015), MathematicaForPrediction at GitHub.
*       URL: https://github.com/antononcube/MathematicaForPrediction/blob/master/Misc/RSparseMatrix.m
*
*   [6] Anton Antonov, Cross tabulation implementation in Mathematica, (2017), MathematicaForPrediction at GitHub.
*
* *)

(*
*   # TODOs
*
*   1. [X] [A] Implement tag-types re-weighting.
*   2. [X] [A] Implement tags re-weighting.
*   3. [ ] [B] Implement term weight functions application through association of tag-types.
*   4. [X] [A] Implement creation from long form dataset, and
*   5. [ ] [A] do corresponding refactoring.
*   6. [ ] [B] Refactor the codes of the term weight functions ("SMRMonApply*Function",).
*
* *)


(**************************************************************)
(* Package definition                                         *)
(**************************************************************)

BeginPackage["AntonAntonov`MonadicSparseMatrixRecommender`SMRMon`"];

Begin["`Private`"];


Needs["AntonAntonov`MonadicSparseMatrixRecommender`"];
Needs["AntonAntonov`MonadicLatentSemanticAnalysis`"];
Needs["AntonAntonov`MonadMakers`"];
Needs["AntonAntonov`DataReshapers`"];
Needs["AntonAntonov`SSparseMatrix`"];
Needs["AntonAntonov`OutlierIdentifiers`"];

(**************************************************************)
(* Generation                                                 *)
(**************************************************************)

(* Generate base functions of SMRMon monad (through StMon.) *)

GenerateStateMonadCode[ "AntonAntonov`MonadicSparseMatrixRecommender`SMRMon", "FailureSymbol" -> $SMRMonFailure, "StringContextNames" -> False ];

GenerateMonadAccessors[
  "AntonAntonov`MonadicSparseMatrixRecommender`SMRMon",
  {"data", "M", "M01", "matrices", "itemNames", "tags", "tagTypeWeights", "timeSeriesMatrix" },
  "FailureSymbol" -> $SMRMonFailure ];

GenerateMonadAccessors[
  "AntonAntonov`MonadicSparseMatrixRecommender`SMRMon",
  {"M", "M01"},
  "DecapitalizeElementName" -> False,
  "FailureSymbol" -> $SMRMonFailure ];

(**************************************************************)
(* Setters / getters                                          *)
(**************************************************************)

(* Here we can/have to have a correctness check. It is one of the advantages to SSparseMatrix. *)
(*SMRMonSetItemNames[names_][xs_, context_Association] :=*)
(*SMRMonUnit[ xs, Join[context, <|"itemNames"->names|>] ];*)

Clear[SMRMonTakeTagTypes];
SMRMonTakeTagTypes[$SMRMonFailure] := $SMRMonFailure;
SMRMonTakeTagTypes[][$SMRMonFailure] := $SMRMonFailure;
SMRMonTakeTagTypes[xs_, context_Association] := SMRMonTakeTagTypes[][xs, context];
SMRMonTakeTagTypes[][xs_, context_Association] := Keys[Lookup[context, "matrices"]];
SMRMonTakeTagTypes[__][___] := $SMRMonFailure;

Clear[SMRMonTakeMatrix];
SMRMonTakeMatrix = SMRMonTakeM;

Clear[SMRMonTakeMatrixDataset];
SMRMonTakeMatrixDataset[$SMRMonFailure] := $SMRMonFailure;
SMRMonTakeMatrixDataset[][$SMRMonFailure] := $SMRMonFailure;
SMRMonTakeMatrixDataset[xs_, context_] := SMRMonTakeMatrixDataset[][xs, context];
SMRMonTakeMatrixDataset[][xs_, context_Association] :=
    Block[{smats},
      smats = SMRMonTakeMatrices[][xs, context];
      If[ TrueQ[smats === $SMRMonFailure],
        $SMRMonFailure,
        (*ELSE*)
        smats =
            Map[
              Dataset[SSparseMatrixToTriplets[#]][All, AssociationThread[{"Item", "Tag", "Weight"} -> #]&]&,
              smats
            ];
        smats = KeyValueMap[ Function[{k, v}, v[All, Join[#, <|"TagType" -> k|>]&]], smats];
        (Join @@ smats)[All, {"Item", "TagType", "Tag", "Weight"}]
      ]
    ];
SMRMonTakeMatrixDataset[__][___] := $SMRMonFailure;


(**************************************************************)
(* Predicates                                                 *)
(**************************************************************)

Clear[ScoredItemIndexesQ];

ScoredItemIndexesQ[recs_Association, context_Association] :=
    If[ KeyExistsQ[context, "M"],
      AssociationQ[recs] &&
          VectorQ[Values[recs], NumberQ] &&
          Apply[And, Map[TrueQ[0 < # <= RowsCount[context["M"]]]&, Keys[recs]] ],
      True,
      (*ELSE*)
      AssociationQ[recs] && VectorQ[Values[recs], NumberQ]
    ];

ScoredItemIndexesQ[___] := False;

Clear[SMRMonScoredItemsQ];

SMRMonScoredItemsQ[$SMRMonFailure] := $SMRMonFailure;

SMRMonScoredItemsQ[xs_, context_Association] := SMRMonScoredItemsQ[xs][xs, context];

SMRMonScoredItemsQ[][xs_, context_Association] := SMRMonScoredItemsQ[xs][xs, context];

SMRMonScoredItemsQ[recs_Association][xs_, context_Association] :=
    Block[{res},
      res =
          If[ KeyExistsQ[context, "itemNames"],
            AssociationQ[recs] &&
                VectorQ[Values[recs], NumberQ] &&
                ( VectorQ[context["itemNames"] /@ Keys[recs], IntegerQ] ||
                    1 <= Min[Keys[recs]] && Max[Keys[recs]] <= Length[context["itemNames"]]
                ),
            (*ELSE*)
            AssociationQ[recs] && VectorQ[Values[recs], NumberQ]
          ];
      SMRMonUnit[res, context]
    ];

SMRMonScoredItemsQ[__][___] := $SMRMonFailure;


Clear[SMRMonScoredTagsQ];

SMRMonScoredTagsQ[$SMRMonFailure] := $SMRMonFailure;

SMRMonScoredTagsQ[xs_, context_Association] := SMRMonScoredTagsQ[xs][xs, context];

SMRMonScoredTagsQ[][xs_, context_Association] := SMRMonScoredTagsQ[xs][xs, context];

SMRMonScoredTagsQ[recs_Association][xs_, context_Association] :=
    Block[{res},
      res =
          If[ KeyExistsQ[context, "tags"],
            AssociationQ[recs] &&
                VectorQ[Values[recs], NumberQ] &&
                ( VectorQ[context["tags"] /@ Keys[recs], IntegerQ] ||
                    1 <= Min[Keys[recs]] && Max[Keys[recs]] <= Length[context["tags"]]
                ),
            (*ELSE*)
            AssociationQ[recs] && VectorQ[Values[recs], NumberQ]
          ];
      SMRMonUnit[res, context]
    ];

SMRMonScoredTagsQ[__][___] := $SMRMonFailure;


(* Private function. *)
Clear[ScoredItemsQ];

ScoredItemsQ[recs_Association, context_Association] :=
    Block[{},
      TrueQ[ Fold[ SMRMonBind, SMRMonUnit[None, context], { SMRMonScoredItemsQ[recs], SMRMonTakeValue}] ]
    ];

ScoredItemsQ[_, context_Association] := False;

ScoredItemsQ[___] := (Echo["Wrong signature!", "ScoredItemsQ:"]; False);


(* Private function. *)
Clear[ScoredTagsQ];

ScoredTagsQ[prof_Association, context_Association] :=
    Block[{},
      TrueQ[ Fold[ SMRMonBind, SMRMonUnit[None, context], { SMRMonScoredTagsQ[prof], SMRMonTakeValue}] ]
    ];

ScoredTagsQ[_, context_Association] := False;

ScoredTagsQ[___] := (Echo["Wrong signature!", "ScoredTagsQ:"]; False);

(* Private function. *)
Clear[DatasetWithScoredItemsQ];

DatasetWithScoredItemsQ[recs_Dataset, context_Association] :=
    Block[{colNames = Normal @ Keys @ recs[[1]] },

      If[ Length[ Intersection[ colNames, {"Item", "Score"} ] ] < 2,
        False,
        ScoredItemsQ[ AssociationThread[ Normal[ recs[All, "Item"] ], Normal[ recs[All, "Score"] ] ], context ]
      ]

    ];

DatasetWithScoredItemsQ[_, context_Association] := False;

DatasetWithScoredItemsQ[___] := (Echo["Wrong signature!", "DatasetWithScoredItemsQ:"]; False);


(**************************************************************)
(* Creation                                                   *)
(**************************************************************)

(* This can be a function MathematicaForPredictionUtilities.m . *)
Clear[NumericalColumns];
NumericalColumns[ds_Dataset] :=
    Block[{aNumColsQ},
      aNumColsQ =
          Normal@ds[
            Transpose /*
                Query[All, VectorQ[DeleteMissing[#], NumericQ] &]];

      Keys[Pick[aNumColsQ, Values[aNumColsQ]]]
    ];

(*
I am trying to avoid the package DateReshape.m here. Using ToLongForm from that package we can do:
ToSSparseMatrix@
 CrossTabulate[ToLongForm[ds[All, {idColumnName, varColumnName}], {idColumnName}, {varColumnName}]]
*)
Clear[NumericalColumnToSSparseMatrix];
NumericalColumnToSSparseMatrix[dsArg_Dataset, idColumnName_, varColumnName_] :=
    Block[{ds = dsArg},

      ds = dsArg[All, {idColumnName, varColumnName}][All, Join[#, <|"Variable" -> varColumnName|>]&];
      ds = Query[ReplaceAll[Missing[] -> 0], All][ds];

      ToSSparseMatrix @ CrossTabulate[ ds[All, {idColumnName, "Variable", varColumnName}], "Sparse" -> True ]
    ];

Clear[SMRMonCreate];

(*SMRMonCreate::rneq = "The row names of SSparseMatrix objects are not the same."*)
(*SMRMonCreate::niid = "The specified item variable name is not one of the column names of the dataset."*)

SyntaxInformation[SMRMonCreate] = { "ArgumentsPattern" -> { _., _., OptionsPattern[] } };

Options[SMRMonCreate] =
    {
      "AddTagTypesToColumnNames" -> False,
      "TagValueSeparator" -> ".",
      "NumericalColumnsAsCategorical" -> False,
      "MissingValuesPattern" -> (None | "None" | Missing[___])
    };

SMRMonCreate[$SMRMonFailure] := $SMRMonFailure;

SMRMonCreate[xs_, context_Association] := SMRMonCreate[][xs, context];

SMRMonCreate[][xs_, context_Association] := SMRMonCreate[Options[SMRMonCreate]][xs, context];

SMRMonCreate[ opts : OptionsPattern[] ][xs_, context_Association] :=
    Which[
      MatrixQ[xs, NumberQ],
      SMRMonCreate[ xs, opts ][xs, context],

      MatchQ[xs, _SSparseMatrix] || MatchQ[xs, Association[ (_ -> _SSparseMatrix) ..] ],
      SMRMonCreate[ xs, opts ][xs, context],

      AssociationQ[xs] && KeyExistsQ[xs, "data"] && KeyExistsQ[xs, "idColumnName"],
      SMRMonCreate[ xs["data"], xs["idColumnName"], opts ][xs, context],

      AssociationQ[xs] && KeyExistsQ[xs, "data"] && KeyExistsQ[xs, "itemColumnName"],
      SMRMonCreate[ xs["data"], xs["itemColumnName"], opts ][xs, context],

      AssociationQ[xs] && KeyExistsQ[xs, "data"],
      SMRMonCreate[ xs["data"], First @ Normal @ Keys @ xs["data"][[1]], opts ][xs, context],

      TrueQ[ Head[xs] === Dataset ],
      SMRMonCreate[ xs, First @ Normal @ Keys @ xs[[1]], opts ][xs, context],

      True,
      SMRMonCreate["Fail"][xs, context]
    ];

SMRMonCreate[ mat_?MatrixQ, opts : OptionsPattern[] ][xs_, context_Association] :=
    Block[{smat},

      smat = ToSSparseMatrix[SparseArray[mat], "RowNames" -> ToString /@ Range[Length[mat]]];

      SMRMonCreate[ <| "anonymous" -> smat |>, opts][xs, context]
    ] /; MatrixQ[ mat, NumberQ ];

SMRMonCreate[ smat_SSparseMatrix, opts : OptionsPattern[] ][xs_, context_Association] :=
    SMRMonCreate[ <| "anonymous" -> smat |>, opts][xs, context];

SMRMonCreate[smatsArg : Association[ (_ -> _SSparseMatrix) ..], opts : OptionsPattern[]][xs_, context_Association] :=
    Block[{smats = smatsArg, addTagTypesToColumnNamesQ, tagValueSeparator, tagTypeNames, rowNames, columnNames, lsCommonColumnNames, splicedMat},

      addTagTypesToColumnNamesQ = TrueQ[OptionValue[SMRMonCreate, "AddTagTypesToColumnNames"]];
      tagValueSeparator = ToString[OptionValue[SMRMonCreate, "TagValueSeparator"]];

      tagTypeNames = Keys[smats];

      rowNames = Map[RowNames, smats];

      If[ !(Equal @@ rowNames),
        (* Echo["The row names of the SSparseMatrix objects are expected to be the same.", "SMRMonCreate:"]; *)
        rowNames = Union[Flatten[Values[rowNames]]];
        smats = Map[ ImposeRowNames[ #, rowNames ]&, smats];
      ];

      If[addTagTypesToColumnNamesQ,

        smats =
            Association @
                KeyValueMap[Function[{k, mat},
                  k -> ToSSparseMatrix[mat,
                    "ColumnNames" -> Map[ k <> tagValueSeparator <> #&, ColumnNames[mat] ],
                    "RowNames" -> RowNames[mat]
                  ]], smats]
      ];

      lsCommonColumnNames =
          Association @ Flatten @
              Map[
                # -> Length[Intersection[ColumnNames@smats[#[[1]]], ColumnNames@smats[#[[2]]]]]&,
                Flatten[ Table[ Keys[smats][[{k1, k2}]], {k1, 1, Length[smats]}, {k2, k1 + 1, Length[smats]}], 1]
              ];
      lsCommonColumnNames = Select[ KeySelect[ lsCommonColumnNames, Not[Equal @@ #]&], # > 0 &];

      If[ Length[lsCommonColumnNames] > 0,
        Echo[
          "Some of the creation sub-matrices have common column names, therefore the columns were renamed." <>
              " Sub-matrices with common column names: " <>
              ToString[lsCommonColumnNames] <>
              " .",
          "SMRMonCreate:" ];
      ];

      splicedMat = ColumnBind[Values[smats]];

      rowNames = RowNames[splicedMat];

      columnNames = ColumnNames[splicedMat];

      SMRMonUnit[
        xs,
        Join[
          context,
          <|
            "itemNames" -> AssociationThread[rowNames -> Range[Length[rowNames]]],
            "tags" -> AssociationThread[columnNames -> Range[Length[columnNames]]],
            "tagTypeWeights" -> AssociationThread[Keys[smats], ConstantArray[1, Length[smats]]],
            "matrices" -> smats,
            "M01" -> splicedMat,
            "M" -> splicedMat
          |>
        ]
      ]
    ];

SMRMonCreate[ds_Dataset, opts : OptionsPattern[]][xs_, context_Association] :=
    Block[{itemVarName, keys},

      keys = Normal @ Keys @ ds[[1]];

      Which[
        MemberQ[ ToLowerCase[keys], "id" ],
        itemVarName = keys[[ First @ Flatten @ Position[ ToLowerCase[keys], "id" ] ]],

        MemberQ[ ToLowerCase[keys], "item" ],
        itemVarName = keys[[ First @ Flatten @ Position[ ToLowerCase[keys], "item" ] ]],

        True,
        itemVarName = First @ Normal @ Keys @ ds[[1]];
      ];

      Echo[ "Heuristically picking the ID column to be \"" <> itemVarName <> "\".", "SMRMonCreate:" ];

      SMRMonCreate[ds, itemVarName, opts][xs, context]
    ];

SMRMonCreate[ds_Dataset, itemVarName_String, opts : OptionsPattern[]][xs_, context_Association] :=
    Block[{ },
      SMRMonCreateFromWideForm[ds, itemVarName, opts][xs, context]
    ];

SMRMonCreate[ds_Dataset, { itemColumnName_String, tagTypeColumnName_String, tagColumnName_String, weightColumnName_String }, opts : OptionsPattern[]][xs_, context_Association] :=
    Block[{ },
      SMRMonCreateFromLongForm[
        ds,
        { itemColumnName, tagTypeColumnName, tagColumnName, weightColumnName },
        FilterRules[ opts, Options[SMRMonCreateFromLongForm] ]
      ][xs, context]
    ];

SMRMonCreate[___][__] :=
    Block[{},
      Echo[
        "The first argument is expected to be a Dataset, a matrix, a SSparseMatrix object, or an association of SSparseMatrix objects. " <>
            "If the first argument is a Dataset a second argument is expected. " <>
            "That second argument should be an item ID column name in that Dataset (wide form) " <>
            "or a list of four strings specifying the column names that correspond to item ID, tag type, tag, weight. " <>
            "If no arguments are given the pipeline value is used.",
        "SMRMonCreate:"];
      $SMRMonFailure
    ];


(**************************************************************)
(* SMRMonCreateFromWideForm                                   *)
(**************************************************************)

Clear[SMRMonCreateFromWideForm];

SyntaxInformation[SMRMonCreateFromWideForm] = { "ArgumentsPattern" -> { _, _, OptionsPattern[] } };

Options[SMRMonCreateFromWideForm] =
    {
      "AddTagTypesToColumnNames" -> False,
      "TagValueSeparator" -> ".",
      "NumericalColumnsAsCategorical" -> False,
      "MissingValuesPattern" -> (None | "None" | Missing[___])
    };

SMRMonCreateFromWideForm[$SMRMonFailure] := $SMRMonFailure;

SMRMonCreateFromWideForm[xs_, context_Association] := $SMRMonFailure;

SMRMonCreateFromWideForm[][xs_, context_Association] := $SMRMonFailure;

SMRMonCreateFromWideForm[ds_Dataset, itemVarName_String, opts : OptionsPattern[]][xs_, context_Association] :=
    Block[{ ncol, tagTypeNames, smats, idPos, idName, numCols, ds2, rowNames, allRowNames,
      numericalColumnsAsCategoricalQ, missingValuesPattern},

      numericalColumnsAsCategoricalQ = TrueQ[OptionValue[SMRMonCreateFromWideForm, "NumericalColumnsAsCategorical"]];
      missingValuesPattern = OptionValue[SMRMonCreateFromWideForm, "MissingValuesPattern"];

      ncol = Dimensions[ds][[2]];

      tagTypeNames = Normal[Keys[ds[1]]];

      idPos = Flatten[Position[tagTypeNames, itemVarName]];
      If[ Length[idPos] == 0,
        Echo["The specified item variable name is not one of the column names of the dataset.", "SMRMonCreate:"];
        Return[$Failed]
      ];
      idPos = First[idPos];
      idName = tagTypeNames[[idPos]];

      If[ numericalColumnsAsCategoricalQ,
        (* This is intentionally separated from the 'else' code in order to avoid the redundant call to NumericalColumns. *)
        smats = Table[ v -> ToSSparseMatrix[ CrossTabulate[ds[All, {idName, v}], "Sparse" -> True ]], {v, Complement[ tagTypeNames, {idName} ]}],

        (*ELSE*)

        numCols = NumericalColumns[ds];

        smats =
            Table[
              If[ MemberQ[numCols, v],
                v -> NumericalColumnToSSparseMatrix[ds, idName, v],
                (*ELSE*)
                ds2 = ds[All, {idName, v}];
                ds2 = ds2[ Select[ FreeQ[#, missingValuesPattern]& ] ];
                v -> ToSSparseMatrix[ CrossTabulate[ds2, "Sparse" -> True] ]
              ], {v, Complement[ tagTypeNames, {idName} ]}];
      ];

      smats = Association[smats];

      SMRMonCreate[smats, opts][xs, Join[context, <|"data" -> ds|>]]
    ];

SMRMonCreateFromWideForm[___][__] :=
    Block[{},
      Echo[
        "The first argument is expected to be a Dataset object. " <>
            "The second argument is expected to be a column name in that Dataset.",
        "SMRMonCreateFromWideForm:"];
      $SMRMonFailure
    ];


(**************************************************************)
(* SMRMonCreateFromLongForm                                   *)
(**************************************************************)

Clear[SMRMonCreateFromLongForm];

SyntaxInformation[SMRMonCreateFromLongForm] = { "ArgumentsPattern" -> { _, ___, OptionsPattern[] } };

Options[SMRMonCreateFromLongForm] =
    {
      "AddTagTypesToColumnNames" -> True,
      "TagValueSeparator" -> ":",
      "MissingValuesPattern" -> (None | "None" | Missing[___])
    };

SMRMonCreateFromLongForm[$SMRMonFailure] := $SMRMonFailure;

SMRMonCreateFromLongForm[xs_, context_Association] := $SMRMonFailure;

SMRMonCreateFromLongForm[][xs_, context_Association] := $SMRMonFailure;

SMRMonCreateFromLongForm[ds_Dataset, opts : OptionsPattern[]][xs_, context_Association] :=
    Block[{defaultColumnNames = { "Item", "TagType", "Tag", "Weight" } },
      Echo["Assuming the long form column names are " <> ToString[defaultColumnNames] <> "."];
      SMRMonCreateFromLongForm[ds, defaultColumnNames, opts ][xs, context]
    ];

SMRMonCreateFromLongForm[ds_Dataset, { itemColumnName_String, tagTypeColumnName_String, tagColumnName_String, weightColumnName_String }, opts : OptionsPattern[]][xs_, context_Association] :=
    Block[{ tagTypeNames, smats,
      addTagTypesToColumnNamesQ, tagValueSeparator, missingValuesPattern},

      addTagTypesToColumnNamesQ = TrueQ[OptionValue[SMRMonCreateFromLongForm, "AddTagTypesToColumnNames"]];
      tagValueSeparator = ToString[OptionValue[SMRMonCreateFromLongForm, "TagValueSeparator"]];
      missingValuesPattern = OptionValue[SMRMonCreateFromLongForm, "MissingValuesPattern"];


      If[ Length[ Intersection[ Normal @ Keys @ ds[[1]], {itemColumnName, tagTypeColumnName, tagColumnName, weightColumnName}  ] ] != 4,
        Echo[
          "Not all of the specified column names are column names in the dataset. " <>
              "The expected dataset column names are:" <> ToString[ {itemColumnName, tagTypeColumnName, tagColumnName, weightColumnName} ] <> ".",
          "SMRMonCreateFromLongForm:"];
        Return[$Failed]
      ];

      tagTypeNames = Union[ Normal[ ds[All, tagTypeColumnName] ] ];

      smats = Association @ Map[Function[{tt}, tt -> ToSSparseMatrix @ CrossTabulate[ ds[ Select[#TagType == tt &], {itemColumnName, tagColumnName, weightColumnName}], "Sparse" -> True]  ], tagTypeNames];

      If[addTagTypesToColumnNamesQ,

        smats =
            Association @
                KeyValueMap[Function[{k, mat},
                  k -> ToSSparseMatrix[mat,
                    "ColumnNames" -> Map[ k <> tagValueSeparator <> #&, ColumnNames[mat] ],
                    "RowNames" -> RowNames[mat]
                  ]], smats]
      ];

      SMRMonCreate[smats][xs, context]
    ];

SMRMonCreateFromLongForm[___][__] :=
    Block[{},
      Echo[
        "The first argument is expected to be a Dataset object. " <>
            "The optional second argument is expected to be a list four strings : " <>
            "{ itemColumnName_String, tagTypeColumnName_String, tagColumnName_String, weightColumnName_String } .",
        "SMRMonCreateFromLongForm:"];
      $SMRMonFailure
    ];


(**************************************************************)
(* SMRMonGetTagTypeRanges                                     *)
(**************************************************************)

Clear[SMRMonGetTagTypeRanges];

SyntaxInformation[SMRMonGetTagTypeRanges] = { "ArgumentsPattern" -> {} };

SMRMonGetTagTypeRanges[$SMRMonFailure] := $SMRMonFailure;

SMRMonGetTagTypeRanges[][$SMRMonFailure] := $SMRMonFailure;

SMRMonGetTagTypeRanges[xs_, context_Association] := SMRMonGetTagTypeRanges[][xs, context];

SMRMonGetTagTypeRanges[][xs_, context_Association] :=
    Block[{lsNCols, aTagTypeRanges},
      lsNCols = Map[ColumnsCount, SMRMonBind[ SMRMonUnit[xs, context], SMRMonTakeMatrices] ];

      aTagTypeRanges = AssociationThread[Keys[lsNCols], Transpose[{Prepend[Most[Accumulate[Values@lsNCols]], 0] + 1, Accumulate[Values@lsNCols]}]];

      SMRMonUnit[ aTagTypeRanges, context]
    ];

SMRMonGetTagTypeRanges[___][__] :=
    Block[{},
      Echo["No arguments are expected.", "SMRMonGetTagTypeRanges:"];
      $SMRMonFailure
    ];


(**************************************************************)
(* SMRMonEchoDataSummary                                      *)
(**************************************************************)

Clear[SMRMonEchoDataSummary];

SyntaxInformation[SMRMonEchoDataSummary] = { "ArgumentsPattern" -> {} };

SMRMonEchoDataSummary[$SMRMonFailure] := $SMRMonFailure;

SMRMonEchoDataSummary[][$SMRMonFailure] := $SMRMonFailure;

SMRMonEchoDataSummary[xs_, context_Association] := SMRMonEchoDataSummary[][xs, context];

SMRMonEchoDataSummary[][xs_, context_Association] :=
    Block[{},
      If[ TrueQ[ SMRMonTakeData[xs, context] === $SMRMonFailure ],
        $SMRMonFailure,
        SMRMonBind[ SMRMonUnit[xs, context], SMRMonEchoFunctionContext[ "data summary:", RecordsSummary[#data] & ] ]
      ]
    ];

SMRMonEchoDataSummary[___][__] :=
    Block[{},
      Echo["No arguments are expected.", "SMRMonEchoDataSummary:"];
      $SMRMonFailure
    ];


(**************************************************************)
(* SMRMonGetProperty                                          *)
(**************************************************************)

Clear[SMRMonGetProperty];

SyntaxInformation[SMRMonGetProperty] = { "ArgumentsPattern" -> {_} };

SMRMonGetProperty[$SMRMonFailure] := $SMRMonFailure;

SMRMonGetProperty[][$SMRMonFailure] := $SMRMonFailure;

SMRMonGetProperty[xs_, context_Association] := SMRMonGetProperty[None][xs, context];

SMRMonGetProperty[ property_String ][xs_, context_Association] :=
    Block[{res, cms},

      res =
          Which[
            ToLowerCase[property] == ToLowerCase["TagTypes"],
            SMRMonBind[ SMRMonUnit[xs, context], SMRMonTakeTagTypes],

            ToLowerCase[property] == ToLowerCase["TagTypeRanges"],
            cms = ColumnsCount /@ SMRMonBind[ SMRMonUnit[xs, context], SMRMonTakeMatrices];
            AssociationThread[ Keys[cms], Rest[ FoldList[{#1[[2]] + 1, #1[[2]] + #2} &, {0, 0}, Values[cms]] ] ],

            ToLowerCase[property] == ToLowerCase["ItemColumnName"],
            SMRMonBind[ SMRMonUnit[xs, context], SMRMonTakeItemColumnName],

            MemberQ[ ToLowerCase[ {"Matrix", "M"} ], ToLowerCase[property] ],
            SMRMonBind[ SMRMonUnit[xs, context], SMRMonTakeM],

            MemberQ[ ToLowerCase[ {"IncidenceMatrix", "M01"} ], ToLowerCase[property] ],
            SMRMonBind[ SMRMonUnit[xs, context], SMRMonTakeM01],

            MemberQ[ ToLowerCase[ {"Matrices", "SubMatrices", "SubMatrixes", "ContingencyMatrices", "ContingencyMatrixes"} ], ToLowerCase[property] ],
            SMRMonBind[ SMRMonUnit[xs, context], SMRMonTakeMatrices],

            ToLowerCase[property] == ToLowerCase["Properties"],
            { "Matrix", "IncidenceMatrix", "SubMatrices", "Properties" },

            True,
            Echo[ "Unknown property specification.", "SMRMonGetProperty:"];
            Return[$SMRMonFailure]
          ];

      SMRMonUnit[ res, context ]
    ];

SMRMonGetProperty[___][__] :=
    Block[{},
      Echo["One string argument is expected.", "SMRMonGetProperty:"];
      $SMRMonFailure
    ];


(**************************************************************)
(* SMRMonGetMatrixProperty                                    *)
(**************************************************************)

Clear[SMRMonGetMatrixProperty];

SyntaxInformation[SMRMonGetMatrixProperty] = { "ArgumentsPattern" -> {_} };

SMRMonGetMatrixProperty[$SMRMonFailure] := $SMRMonFailure;

SMRMonGetMatrixProperty[][$SMRMonFailure] := $SMRMonFailure;

SMRMonGetMatrixProperty[xs_, context_Association] := SMRMonGetMatrixProperty[None][xs, context];

SMRMonGetMatrixProperty[ property_String ][xs_, context_Association] :=
    Block[{res},

      res =
          Which[
            MemberQ[ ToLowerCase[ {"Tags", "Columns"} ], ToLowerCase[property] ],
            ColumnNames[ SMRMonBind[ SMRMonUnit[xs, context], SMRMonTakeM] ],

            ToLowerCase[property] == ToLowerCase["Rows"],
            RowNames[ SMRMonBind[ SMRMonUnit[xs, context], SMRMonTakeM] ],

            MemberQ[ ToLowerCase[ {"NumberOfColumns", "ColumnsCount"} ], ToLowerCase[property] ],
            ColumnsCount[ SMRMonBind[ SMRMonUnit[xs, context], SMRMonTakeM] ],

            MemberQ[ ToLowerCase[ {"NumberOfRows", "ColumnsRows"} ], ToLowerCase[property] ],
            RowsCount[ SMRMonBind[ SMRMonUnit[xs, context], SMRMonTakeM] ],

            MemberQ[ ToLowerCase[ {"Dimensions", "Dim"} ], ToLowerCase[property] ],
            Dimensions[ SparseArray[ SMRMonBind[ SMRMonUnit[xs, context], SMRMonTakeM] ] ],

            ToLowerCase[property] == ToLowerCase["Density"],
            SparseArray[ SMRMonBind[ SMRMonUnit[xs, context], SMRMonTakeM] ]["Density"],

            MemberQ[ ToLowerCase[ {"TagTypeWeights", "TagTypeSignificanceFactors"} ], ToLowerCase[property] ],
            SMRMonBind[ SMRMonUnit[xs, context], SMRMonTakeTagTypeWeights],

            ToLowerCase[property] == ToLowerCase["Properties"],
            { "Tags", "Columns", "Rows",
              "NumberOfColumns", "NumberOfRows",
              "Dim", "Dimensions", "Density",
              "TagTypeWeights", "TagTypeSignificanceFactors",
              "Properties" },

            True,
            Echo[ "Unknown property specification.", "SMRMonGetMatrixProperty:"];
            Return[$SMRMonFailure]
          ];

      SMRMonUnit[ res, context ]
    ];

SMRMonGetMatrixProperty[___][__] :=
    Block[{},
      Echo["One string argument is expected.", "SMRMonGetMatrixProperty:"];
      $SMRMonFailure
    ];


(**************************************************************)
(* SMRMonFilterByProfile                                      *)
(**************************************************************)

Clear[SMRMonFilterByProfile];

SyntaxInformation[SMRMonFilterByProfile] = { "ArgumentsPattern" -> {_, OptionsPattern[]} };

Options[SMRMonFilterByProfile] = { "Type" -> "Intersection" };

SMRMonFilterByProfile[$SMRMonFailure] := $SMRMonFailure;

SMRMonFilterByProfile[][$SMRMonFailure] := $SMRMonFailure;

SMRMonFilterByProfile[xs_, context_Association] := SMRMonFilterByProfile[None][xs, context];

SMRMonFilterByProfile[ profile_Association, opts : OptionsPattern[] ][xs_, context_Association] :=
    SMRMonFilterByProfile[ Keys[profile], opts][xs, context];

SMRMonFilterByProfile[ profile_String, opts : OptionsPattern[] ][xs_, context_Association] :=
    SMRMonFilterByProfile[ {profile}, opts][xs, context];

SMRMonFilterByProfile[ profile : {_String ..}, opts : OptionsPattern[] ][xs_, context_Association] :=
    Block[{ filterType, pvec, svec, rowInds, res },

      filterType = OptionValue[ SMRMonFilterByProfile, "Type" ];

      pvec = Fold[ SMRMonBind, SMRMonUnit[xs, context], { SMRMonToProfileVector[profile], SMRMonTakeValue } ];
      If[ TrueQ[ pvec === $SMRMonFailure ],
        Echo["Cannot make a profile vector.", "SMRMonFilterByProfile:"];
        Return[$SMRMonFailure]
      ];

      pvec = Unitize[pvec];

      Which[
        StringQ[filterType] && ToLowerCase[filterType] == "union",
        svec = context["M"] . pvec;
        rowInds = Flatten @ Most[ ArrayRules[svec] ][[All, 1]],

        StringQ[filterType] && ToLowerCase[filterType] == "intersection",
        svec = Unitize[context["M"]] . pvec;
        rowInds = Flatten @ Select[ Most @ ArrayRules[svec], #[[2]] == Total[pvec]& ][[All, 1]],

        True,
        Echo["The value of the option \"Type\" should be either \"Union\" or \"Intersection\".", "SMRMonFilterByProfile:"];
        Return[$SMRMonFailure]
      ];

      res = ReverseSort @ RowSumsAssociation[ Unitize @ context["M"][[ rowInds, All ]]];

      SMRMonUnit[res, context]
    ];

SMRMonFilterByProfile[___][__] :=
    Block[{},
      Echo[
        "The expected signature is SMRMonFilterByProfile[ profile : ( _String | {_String ..} | Association[ (_String -> _?NumberQ) .. ] ) ] .",
        "SMRMonFilterByProfile:"
      ];
      $SMRMonFailure
    ];


(**************************************************************)
(* SMRMonFilterMatrix                                         *)
(**************************************************************)

Clear[SMRMonFilterMatrix];

SyntaxInformation[SMRMonFilterMatrix] = { "ArgumentsPattern" -> {_, OptionsPattern[]} };

Options[SMRMonFilterMatrix] = Options[SMRMonFilterByProfile];

SMRMonFilterMatrix[$SMRMonFailure] := $SMRMonFailure;

SMRMonFilterMatrix[][$SMRMonFailure] := $SMRMonFailure;

SMRMonFilterMatrix[xs_, context_Association] := SMRMonFilterMatrix[None][xs, context];

SMRMonFilterMatrix[ profile_Association, opts : OptionsPattern[] ][xs_, context_Association] :=
    SMRMonFilterMatrix[ Keys[profile], opts][xs, context];

SMRMonFilterMatrix[ profile_String, opts : OptionsPattern[] ][xs_, context_Association] :=
    SMRMonFilterMatrix[ {profile}, opts][xs, context];

SMRMonFilterMatrix[ profile : {_String ..}, opts : OptionsPattern[] ][xs_, context_Association] :=
    Block[{ filterType, rowInds, smats },

      filterType = OptionValue[ SMRMonFilterMatrix, "Type" ];

      If[ !( StringQ[filterType] && MemberQ[ {"intersection", "union"}, ToLowerCase[filterType] ] ),
        Echo["The value of the option \"Type\" should be either \"Union\" or \"Intersection\".", "SMRMonFilterMatrix:"];
        Return[$SMRMonFailure]
      ];

      rowInds = Fold[ SMRMonBind, SMRMonUnit[xs, context], { SMRMonFilterByProfile[profile, opts], SMRMonTakeValue } ];

      If[ TrueQ[ rowInds === $SMRMonFailure ], Return[$SMRMonFailure] ];

      rowInds = Keys[rowInds];

      If[ KeyExistsQ[context, "matrices" ],
        smats = Map[ #[[ rowInds, All ]]&, context["matrices"] ],
        (*ELSE*)
        smats = None
      ];

      (* I do not see a benefit of using the monadic solution instead of the direct one. *)
      (*
      Fold[
        SMRMonBind,
        SMRMonUnit[ xs, context ],
        {
          SMRMonSetM[ context["M"][[ rowInds, All ]] ],
          SMRMonSetM01[ context["M01"][[ rowInds, All ]] ]
        }
      ]
      *)
      SMRMonUnit[xs, Join[ context, <| "M" -> context["M"][[ rowInds, All ]], "M01" -> context["M01"][[ rowInds, All ]], "matrices" -> smats |> ] ]
    ];

SMRMonFilterMatrix[___][__] :=
    Block[{},
      Echo[
        "The expected signature is SMRMonFilterMatrix[ profile : ( _String | {_String ..} | Association[ (_String -> _?NumberQ) .. ] ) ] .",
        "SMRMonFilterMatrix:"];
      $SMRMonFailure
    ];


(**************************************************************)
(* SMRMonRetrieveByQueryElements                             *)
(**************************************************************)

Clear[QueryElementSpecQ];
QueryElementSpecQ[x_] := MatchQ[ x, Automatic | None | _String | _List | _Association ];

Clear[QueryElementSpecConvert];
QueryElementSpecConvert[None] := {};
QueryElementSpecConvert[x_String] := {x};
QueryElementSpecConvert[x_Association] := Keys[x];
QueryElementSpecConvert[Automatic] := {};
QueryElementSpecConvert[x_] := x;

Clear[SMRMonRetrieveByQueryElements];

SyntaxInformation[SMRMonRetrieveByQueryElements] = { "ArgumentsPattern" -> {_., _., _., OptionsPattern[]} };

Options[SMRMonRetrieveByQueryElements] = {
  "Should" -> Automatic,
  "Must" -> Automatic, "MustNot" -> Automatic,
  "MustType" -> "Intersection", "MustNotType" -> "Union"};

SMRMonRetrieveByQueryElements[$SMRMonFailure] := $SMRMonFailure;

SMRMonRetrieveByQueryElements[][$SMRMonFailure] := $SMRMonFailure;

SMRMonRetrieveByQueryElements[xs_, context_Association] :=
    SMRMonRetrieveByQueryElements[Options[SMRMonRetrieveByQueryElements]][xs, context];

SMRMonRetrieveByQueryElements[opts : OptionsPattern[]][xs_, context_Association] :=
    Block[{should, must, mustNot},

      should = OptionValue[ SMRMonRetrieveByQueryElements, "Should" ];
      If[ !QueryElementSpecQ[should],
        Echo[
          "The value of the option \"Should\" should be a list, or an association, or Automatic, or None.",
          "SMRMonRetrieveByQueryElements:"];
        Return[$SMRMonFailure]
      ];

      must = OptionValue[ SMRMonRetrieveByQueryElements, "Must" ];
      If[ !QueryElementSpecQ[should],
        Echo[
          "The value of the option \"Must\" should be a list, or an association, or Automatic, or None.",
          "SMRMonRetrieveByQueryElements:"];
        Return[$SMRMonFailure]
      ];

      mustNot = OptionValue[ SMRMonRetrieveByQueryElements, "MustNot" ];
      If[ !QueryElementSpecQ[mustNot],
        Echo[
          "The value of the option \"MustNot\" should be a list, or an association, or Automatic, or None.",
          "SMRMonRetrieveByQueryElements:"];
        Return[$SMRMonFailure]
      ];

      SMRMonRetrieveByQueryElements[should, must, mustNot, opts][xs, context]
    ];

SMRMonRetrieveByQueryElements[should_?QueryElementSpecQ, opts : OptionsPattern[] ][xs_, context_Association] :=
    SMRMonRetrieveByQueryElements[
      should,
      OptionValue[SMRMonRetrieveByQueryElements, "Must"],
      OptionValue[SMRMonRetrieveByQueryElements, "MustNot"], opts ][xs, context];

SMRMonRetrieveByQueryElements[should_?QueryElementSpecQ, must_?QueryElementSpecQ, opts : OptionsPattern[] ][xs_, context_Association] :=
    SMRMonRetrieveByQueryElements[
      should,
      must,
      OptionValue[SMRMonRetrieveByQueryElements, "MustNot"], opts ][xs, context];

SMRMonRetrieveByQueryElements[
  shouldArg_?QueryElementSpecQ,
  mustArg_?QueryElementSpecQ,
  mustNotArg_?QueryElementSpecQ,
  opts : OptionsPattern[] ][xs_, context_Association] :=
    Block[{ should = shouldArg, must = mustArg, mustNot = mustNotArg,
      mustType, mustNotType,
      pvecShould, pvecMust, shouldItems, mustItems, mustNotItems, res },

      mustType = OptionValue[ SMRMonRetrieveByQueryElements, "MustType" ];
      If[ !( StringQ[mustType] && MemberQ[ToLowerCase /@ {"Intersection", "Union"}, ToLowerCase[mustType]]),
        Echo[
          "The value of the option \"MustType\" is expected to be one of \"Intersection\" or \"Union\".",
          "SMRMonRetrieveByQueryElements:"];
        Return[$SMRMonFailure]
      ];

      mustNotType = OptionValue[ SMRMonRetrieveByQueryElements, "MustNotType" ];
      If[ !( StringQ[mustNotType] && MemberQ[ToLowerCase /@ {"Intersection", "Union"}, ToLowerCase[mustNotType]]),
        Echo[
          "The value of the option \"MustNotType\" is expected to be one of \"Intersection\" or \"Union\".",
          "SMRMonRetrieveByQueryElements:"];
        Return[$SMRMonFailure]
      ];

      If[ TrueQ[should === Automatic] && QueryElementSpecQ[xs], should = xs ];
      should = QueryElementSpecConvert[should];
      must = QueryElementSpecConvert[must];
      mustNot = QueryElementSpecConvert[mustNot];

      (* Should *)
      If[ Length[should] > 0 || Length[must] > 0,

        pvecShould = Fold[ SMRMonBind, SMRMonUnit[xs, context], {SMRMonToProfileVector[should], SMRMonTakeValue} ];
        If[ TrueQ[pvecShould === $SMRMonFailure], Return[$SMRMonFailure]];

        pvecMust = Fold[ SMRMonBind, SMRMonUnit[xs, context], {SMRMonToProfileVector[must], SMRMonTakeValue} ];
        If[ TrueQ[pvecMust === $SMRMonFailure], Return[$SMRMonFailure]];

        shouldItems = Fold[ SMRMonBind, SMRMonUnit[xs, context], { SMRMonRecommendByProfile[ pvecShould + pvecMust, All], SMRMonTakeValue }];
        If[ TrueQ[pvecShould === $SMRMonFailure], Return[$SMRMonFailure]],

        (* ELSE *)
        shouldItems = RowSumsAssociation @ context["M"];
      ];

      res = shouldItems;

      (* Must *)
      If[ Length[must] > 0,
        mustItems = Fold[SMRMonBind, SMRMonUnit[xs, context], {SMRMonFilterByProfile[must, "Type" -> mustType ], SMRMonTakeValue}],
        (*ELSE*)
        mustItems = <||>
      ];

      If[ Length[must] > 0,
        res = KeyTake[ res, Keys[mustItems] ]
      ];

      (* MustNot *)
      If[ Length[mustNot] > 0,
        mustNotItems = Fold[SMRMonBind, SMRMonUnit[xs, context], {SMRMonFilterByProfile[mustNot, "Type" -> mustNotType ], SMRMonTakeValue}],
        (*ELSE*)
        mustNotItems = <||>
      ];

      If[ Length[mustNot] > 0,
        res = KeyDrop[ res, Keys[mustNotItems] ]
      ];

      SMRMonUnit[res, context]
    ];

SMRMonRetrieveByQueryElements[___][__] :=
    Block[{},
      Echo[
        "The expected signature is SMRMonRetrieveByQueryElements[ should_, must_, mustNot_ ] " <>
            "all arguments matching ( Automatic | None | _String | _List | _Association ).",
        "SMRMonRetrieveByQueryElements:"];
      $SMRMonFailure
    ];


(**************************************************************)
(* SMRMonApplyLocalWeightFunction                             *)
(**************************************************************)

Clear[SMRMonApplyLocalWeightFunction];

SyntaxInformation[SMRMonApplyLocalWeightFunction] = { "ArgumentsPattern" -> {_} };

SMRMonApplyLocalWeightFunction[$SMRMonFailure] := $SMRMonFailure;

SMRMonApplyLocalWeightFunction[][xs_, context_Association] := $SMRMonFailure;

SMRMonApplyLocalWeightFunction[funcName_String][xs_, context_Association] :=
    Block[{mat, smats },

      If[!KeyExistsQ[context, "matrices"],
        Echo["Cannot find the recommendation sub-matrices. (The context key \"matrices\".)", "SMRMonApplyLocalWeightFunction:"];
        Return[$SMRMonFailure]
      ];

      smats =
          Map[
            ToSSparseMatrix[
              ApplyLocalTermFunction[SparseArray[#], funcName],
              "RowNames" -> RowNames[#],
              "ColumnNames" -> ColumnNames[#]
            ]&,
            context["matrices"]
          ];

      mat = ColumnBind[ Values[smats] ];

      SMRMonUnit[xs, Join[ context, <| "matrices" -> smats, "M" -> mat, "M01" -> mat |> ]]
    ];

SMRMonApplyLocalWeightFunction[___][__] := $SMRMonFailure;


(**************************************************************)
(* SMRMonApplyGlobalWeightFunction                            *)
(**************************************************************)

Clear[SMRMonApplyGlobalWeightFunction];

SyntaxInformation[SMRMonApplyGlobalWeightFunction] = { "ArgumentsPattern" -> {_} };

SMRMonApplyGlobalWeightFunction[$SMRMonFailure] := $SMRMonFailure;

SMRMonApplyGlobalWeightFunction[][xs_, context_Association] := $SMRMonFailure;

SMRMonApplyGlobalWeightFunction[funcName_String][xs_, context_Association] :=
    Block[{mat, smats},

      If[!KeyExistsQ[context, "matrices"],
        Echo["Cannot find the recommendation sub-matrices. (The context key \"matrices\".)", "SMRMonApplyGlobalWeightFunction:"];
        Return[$SMRMonFailure]
      ];

      (* Quicker, but we have to keep the correspondence between "matrices" and "M" and "M01". *)
      (*mat = ApplyGlobalWeightFunction[ SparseArray[context["M"]], funcName];*)
      (*mat = ToSSparseMatrix[ mat, "RowNames" -> RowNames[context["M"]], "ColumnNames" -> ColumnNames[context["M"]] ];*)

      smats =
          Map[
            ToSSparseMatrix[
              ApplyGlobalTermFunction[SparseArray[#], funcName],
              "RowNames" -> RowNames[#],
              "ColumnNames" -> ColumnNames[#]
            ]&,
            context["matrices"] ];

      mat = ColumnBind[ Values[smats] ];

      SMRMonUnit[xs, Join[ context, <| "matrices" -> smats, "M" -> mat, "M01" -> mat |> ]]
    ];

SMRMonApplyGlobalWeightFunction[___][__] := $SMRMonFailure;


(**************************************************************)
(* SMRMonApplyNormalizationFunction                           *)
(**************************************************************)

Clear[SMRMonApplyNormalizationFunction];

SyntaxInformation[SMRMonApplyNormalizationFunction] = { "ArgumentsPattern" -> {_} };

SMRMonApplyNormalizationFunction[$SMRMonFailure] := $SMRMonFailure;

SMRMonApplyNormalizationFunction[][xs_, context_Association] := $SMRMonFailure;

SMRMonApplyNormalizationFunction[funcName_String][xs_, context_Association] :=
    Block[{mat, smats},

      If[!KeyExistsQ[context, "matrices"],
        Echo["Cannot find the recommendation sub-matrices. (The context key \"matrices\".)", "SMRMonApplyNormalizationFunction:"];
        Return[$SMRMonFailure]
      ];

      smats =
          Map[
            ToSSparseMatrix[
              ApplyNormalizationFunction[SparseArray[#], funcName],
              "RowNames" -> RowNames[#],
              "ColumnNames" -> ColumnNames[#]
            ]&,
            context["matrices"] ];

      mat = ColumnBind[ Values[smats] ];

      SMRMonUnit[xs, Join[ context, <| "matrices" -> smats, "M" -> mat, "M01" -> mat |> ]]
    ];

SMRMonApplyNormalizationFunction[___][__] := $SMRMonFailure;


(**************************************************************)
(* SMRMonApplyNormalizationFunction                           *)
(**************************************************************)

Clear[SMRMonApplyTermWeightFunctions];

SyntaxInformation[SMRMonApplyTermWeightFunctions] = { "ArgumentsPattern" -> { _., _., _., OptionsPattern[] } };

Options[SMRMonApplyTermWeightFunctions] = { "GlobalWeightFunction" -> "IDF", "LocalWeightFunction" -> "None", "NormalizerFunction" -> "Cosine" };

SMRMonApplyTermWeightFunctions[$SMRMonFailure] := $SMRMonFailure;

SMRMonApplyTermWeightFunctions[___][$SMRMonFailure] := $SMRMonFailure;

SMRMonApplyTermWeightFunctions[xs_, context_Association] := SMRMonApplyTermWeightFunctions[][xs, context];

SMRMonApplyTermWeightFunctions[][xs_, context_Association] := SMRMonApplyTermWeightFunctions["IDF", "None", "Cosine"][xs, context];

SMRMonApplyTermWeightFunctions[ opts : OptionsPattern[] ][xs_, context_Association] :=
    Block[{ termFuncs, val },

      termFuncs =
          Table[
            (
              val = OptionValue[ SMRMonApplyTermWeightFunctions, funcName ];

              If[ ! StringQ[val],
                Echo[
                  "The value of the option \"" <> funcName <> "\" is expected to be a string.",
                  "SMRMonApplyTermWeightFunctions:"
                ];
                Return[$SMRMonFailure]
              ];

              val
            ),
            { funcName, { "GlobalWeightFunction", "LocalWeightFunction", "NormalizerFunction" } }
          ];

      SMRMonApplyTermWeightFunctions[ Sequence @@ termFuncs ][xs, context]
    ];


SMRMonApplyTermWeightFunctions[globalWeightFunction_String : "IDF", localWeightFunction_String : "None", normalizerFunction_String : "Cosine"][xs_, context_Association] :=
    Block[{mat, smats},

      If[!KeyExistsQ[context, "matrices"],
        Echo["Cannot find the recommendation sub-matrices. (The context key \"matrices\".)", "SMRMonApplyTermWeightFunctions:"];
        Return[$SMRMonFailure]
      ];

      smats =
          Map[
            ToSSparseMatrix[
              WeightTerms[SparseArray[#], globalWeightFunction, localWeightFunction, normalizerFunction],
              "RowNames" -> RowNames[#],
              "ColumnNames" -> ColumnNames[#]
            ]&,
            context["matrices"] ];

      mat = ColumnBind[ Values[smats] ];

      SMRMonUnit[xs, Join[ context, <| "matrices" -> smats, "M" -> mat, "M01" -> mat |> ]]
    ];

SMRMonApplyTermWeightFunctions[___][__] := $SMRMonFailure;


(**************************************************************)
(* SMRMonGetTopRecommendations                                *)
(**************************************************************)

Clear[SMRMonGetTopRecommendations];

SyntaxInformation[SMRMonGetTopRecommendations] = { "ArgumentsPattern" -> { _, _., OptionsPattern[] } };

Options[SMRMonGetTopRecommendations] = {
  "Specification" -> None, "NumberOfRecommendations" -> 12,
  "RemoveHistory" -> True, "ItemNames" -> True, "IgnoreUnknownTags" -> False, "Normalize" -> True, "VectorResult" -> False };

SMRMonGetTopRecommendations[$SMRMonFailure] := $SMRMonFailure;

SMRMonGetTopRecommendations[xs_, context_Association] := $SMRMonFailure;

SMRMonGetTopRecommendations[ opts : OptionsPattern[]][xs_, context_Association] :=
    Block[{spec, nrecs},

      spec = OptionValue[SMRMonGetTopRecommendations, "Specification"];

      If[ TrueQ[spec === None] || TrueQ[spec === Automatic],
        spec = xs
      ];

      nrecs = OptionValue[SMRMonGetTopRecommendations, "NumberOfRecommendations"];

      If[ TrueQ[nrecs === Automatic], nrecs = 12 ];

      If[ !( IntegerQ[nrecs] && nrecs > 0 || TrueQ[ nrecs === All ]),
        Echo[
          "The value of the option \"NumberOfRecommendations\" is expected to be a positive integer or All.",
          "SMRMonGetTopRecommendations:"];
        Return[$SMRMonFailure]
      ];

      SMRMonGetTopRecommendations[ spec, nrecs, opts ][xs, context]
    ];

SMRMonGetTopRecommendations[ spec_, opts : OptionsPattern[]][xs_, context_Association] :=
    SMRMonGetTopRecommendations[ spec, 12, opts ][xs, context];

SMRMonGetTopRecommendations[ spec_, nRes : (_Integer | All), opts : OptionsPattern[]][xs_, context_Association] :=
    Block[{res},

      If[ TrueQ[spec === None] || TrueQ[spec === Empty] || TrueQ[spec === {}] || TrueQ[spec === <||>],

        (* Of course we can just use xs instead of SMRMonTakeValue[][xs, context] . *)
        Block[{Echo = Null&},
          res = SMRMonRecommend[ xs, nRes, FilterRules[{opts}, Options[SMRMonRecommend]] ][xs, context]
        ];

        If[ ! ( TrueQ[res === $SMRMonFailure] || Length[res[[1]]] == 0 ), Return[res] ];

        Block[{Echo = Null&},
          res = SMRMonRecommendByProfile[ xs, nRes, FilterRules[{opts}, Options[SMRMonRecommendByProfile]] ][xs, context]
        ];

        If[ TrueQ[res === $SMRMonFailure],
          Echo[ "The monad object value is not a history or profile specification.", "SMRMonGetTopRecommendations:" ];
        ],
        (* ELSE *)

        Block[{Echo = Null&},
          res = SMRMonRecommend[ spec, nRes, FilterRules[{opts}, Options[SMRMonRecommend]] ][xs, context]
        ];

        If[ ! ( TrueQ[res === $SMRMonFailure] || Length[res[[1]]] == 0 ), Return[res] ];

        Block[{Echo = Null&},
          res = SMRMonRecommendByProfile[ spec, nRes, FilterRules[{opts}, Options[SMRMonRecommendByProfile]] ][xs, context]
        ];

        If[ TrueQ[res === $SMRMonFailure],
          Echo[ "The first argument is not a history or profile specification.", "SMRMonGetTopRecommendations:" ];
        ]

      ];

      res
    ];

SMRMonGetTopRecommendations[___][__] :=
    Block[{},
      Echo[
        "The first argument is expected to be None or an association of scored items or scored tags. " <>
            "The second argument is expected to be a positive integer or All.",
        "SMRMonGetTopRecommendations:"];
      $SMRMonFailure
    ];


(**************************************************************)
(* SMRMonRecommend                                            *)
(**************************************************************)

Clear[GetFilterIDs];

GetFilterIDs[context_Association, callerFunctionName_String] :=
    Block[{},
      Which[
        !KeyExistsQ[context, "filter"],
        Echo["There is no key \"filter\" in the context.", callerFunctionName <> ":"];
        All,

        AssociationQ[context["filter"]] && ScoredItemsQ[context["filter"], context],
        Keys[context["filter"]],

        ListQ[context["filter"]] && ScoredItemsQ[AssociationThread[context["filter"], 1], context],
        context["filter"],

        True,
        Echo["The value for the key \"filter\" in the context is expected to a list of items or an association of scored items.",
          callerFunctionName <> ":"];
        All
      ]
    ];

Clear[SMRMonRecommend];

SyntaxInformation[SMRMonRecommend] = { "ArgumentsPattern" -> { _, _., OptionsPattern[] } };

Options[SMRMonRecommend] = {"RemoveHistory" -> True, "ItemNames" -> True, "Normalize" -> True, "VectorResult" -> False };

SMRMonRecommend[$SMRMonFailure] := $SMRMonFailure;

SMRMonRecommend[xs_, context_Association] := $SMRMonFailure;

SMRMonRecommend[ history : ( Association[ (_String -> _?NumberQ) ... ] | _String | {_String..} ), opts : OptionsPattern[]][xs_, context_Association] :=
    SMRMonRecommend[ history, 12, opts][xs, context];

SMRMonRecommend[ history : Association[ (_String -> _?NumberQ) ... ], nRes : (_Integer | All), opts : OptionsPattern[]][xs_, context_Association] :=
    Block[{h},

      h = KeyMap[ context["itemNames"][#]&, history ];
      h = KeySelect[ h, IntegerQ ];

      Which[
        Length[h] == 0,
        Echo["None of the item names is not known by the recommender.", "SMRMonRecommend:"],

        Length[h] < Length[history],
        Echo["Some of the item names are not known by the recommender.", "SMRMonRecommend:"]
      ];

      SMRMonRecommend[ Keys[h], Values[h], nRes, opts][xs, context]
    ];

SMRMonRecommend[ itemName_String, nRes : (_Integer | All), opts : OptionsPattern[]][xs_, context_Association] :=
    SMRMonRecommend[ AssociationThread[{itemName}, 1], nRes, opts][xs, context];

SMRMonRecommend[ itemNames : {_String...}, nRes : (_Integer | All), opts : OptionsPattern[]][xs_, context_Association] :=
    SMRMonRecommend[ AssociationThread[itemNames, 1], nRes, opts][xs, context];

SMRMonRecommend[ history : Association[ (_Integer -> _?NumberQ) ... ], nRes : (_Integer | All), opts : OptionsPattern[]][xs_, context_Association] :=
    SMRMonRecommend[ Keys[history], Values[history], nRes, opts][xs, context];

SMRMonRecommend[ itemIndices : {_Integer...}, nRes : (_Integer | All), opts : OptionsPattern[]][xs_, context_Association] :=
    SMRMonRecommend[ itemIndices, ConstantArray[1, Length[itemIndices]], nRes, opts][xs, context];

SMRMonRecommend[ itemIndices : {_Integer...}, itemRatings : {_?NumberQ...}, nRes : (_Integer | All), opts : OptionsPattern[]][xs_, context_Association] :=
    Block[{removeHistoryQ, itemNamesQ, normalizeQ, vectorResultQ, vec, filterIDs = All,
      filterInds, smat, fmat, recs, recsInds, recsVals, ordInds, rowNames},

      If[Length[itemIndices] == 0,
        Echo["Empty history as an argument.", "SMRMonRecommend:"];
        Return[ SMRMonUnit[<||>, context] ]
      ];

      removeHistoryQ = TrueQ[OptionValue[SMRMonRecommend, "RemoveHistory"]];
      itemNamesQ = TrueQ[OptionValue[SMRMonRecommend, "ItemNames"]];
      normalizeQ = TrueQ[OptionValue[SMRMonRecommend, "Normalize"]];
      vectorResultQ = TrueQ[OptionValue[SMRMonRecommend, "VectorResult"]];

      If[!KeyExistsQ[context, "M"],
        Echo["Cannot find the recommendation matrix. (The context key \"M\".)", "SMRMonRecommend:"];
        Return[$SMRMonFailure]
      ];

      smat = SparseArray[context["M"]];

      (*1*)
      vec = SparseArray[Thread[itemIndices -> itemRatings], {Length[smat]}];

      (*2*)
      vec = smat.(vec.smat);

      If[ KeyExistsQ[context, "filter"],
        filterIDs = GetFilterIDs[context, "SMRMonRecommend"];
        rowNames = RowNamesAssociation[context["M"]];
        filterInds = Join[itemIndices, rowNames[#]& /@ filterIDs ];
        fmat = DiagonalMatrix[ SparseArray[Thread[filterInds -> 1.], RowsCount[context["M"]]] ];
        vec = fmat . vec ;
      ];

      (*3 and 4 and 5*)

      If[ normalizeQ && Max[Abs[vec]] > 0,
        vec = vec / Max[vec];
      ];

      If[ vectorResultQ,

        If[ IntegerQ[ nRes ],
          recsInds = Reverse[ Ordering[ Normal[vec] ] ][[ 1 ;; nRes ]];
          vec = SparseArray[ Thread[ recsInds -> vec[[recsInds]] ], RowsCount[context["M"]] ];
        ];

        Return[ SMRMonUnit[ vec, context ] ]
      ];

      If[ removeHistoryQ,
        vec[[itemIndices]] = 0;
        (* This is probably redundant -- Ordering won't be slowed-down that much. *)
        (* vec = SparseArray[vec]; *)
      ];

      recs = Most[ArrayRules[vec]];

      recsInds = Flatten[recs[[All, 1]]];
      recsVals = recs[[All, 2]];

      ordInds = Reverse[ Ordering[ recsVals ] ];

      If[ IntegerQ[nRes],
        ordInds = Take[ ordInds, UpTo[nRes] ]
      ];

      recsInds = recsInds[[ordInds]];
      recsVals = recsVals[[ordInds]];

      If[itemNamesQ,
        rowNames = RowNames[ context["M"] ];
        recs = AssociationThread[ rowNames[[recsInds]], recsVals ],
        (* ELSE *)
        recs = AssociationThread[ recsInds, recsVals ];
      ];

      SMRMonUnit[recs, context]

    ] /; Length[itemIndices] == Length[itemRatings];


SMRMonRecommend[___][__] :=
    Block[{},
      Echo[
        "The first argument is expected to be an association of scored items. " <>
            "The second argument is expected to be a positive integer or All. " <>
            "The items are recommendation matrix row indices or row names. The scores are positive numbers.",
        "SMRMonRecommend:"];
      $SMRMonFailure
    ];

Clear[SMRMonRecommendByHistory ];
SMRMonRecommendByHistory = SMRMonRecommend;


(**************************************************************)
(* SMRMonRecommendByProfile                                   *)
(**************************************************************)

Clear[SMRMonRecommendByProfile];

SyntaxInformation[SMRMonRecommendByProfile] = { "ArgumentsPattern" -> { _, _., _., OptionsPattern[] } };

Options[SMRMonRecommendByProfile] = {"ItemNames" -> True, "Normalize" -> True, "IgnoreUnknownTags" -> False, "VectorResult" -> False };

SMRMonRecommendByProfile[$SMRMonFailure] := $SMRMonFailure;

SMRMonRecommendByProfile[xs_, context_Association] := $SMRMonFailure;

SMRMonRecommendByProfile[tagsArg : (_?AssociationQ | _List | {_Integer..}), opts : OptionsPattern[]][xs_, context_Association] :=
    SMRMonRecommendByProfile[tagsArg, 12, opts][xs, context];

SMRMonRecommendByProfile[nRes : (_Integer | All), opts : OptionsPattern[]][xs_, context_Association] :=
    Block[{},
      (* Without verification. *)
      SMRMonRecommendByProfile[xs, nRes, opts][xs, context]
    ];

SMRMonRecommendByProfile[profileInds : {_Integer..}, profileScores : {_?NumberQ..}, nRes : (_Integer | All), opts : OptionsPattern[]][xs_, context_Association] :=
    Block[{vec, smat},

      If[!KeyExistsQ[context, "M"],
        Echo["Cannot find the recommendation matrix. (The context key \"M\".)", "SMRMonRecommend:"];
        Return[$SMRMonFailure]
      ];

      smat = SparseArray[context["M"]];

      vec = SparseArray[Thread[profileInds -> profileScores], {Dimensions[smat][[2]]}];

      SMRMonRecommendByProfile[vec, nRes, opts][xs, context]
    ] /; Length[profileInds] == Length[profileScores];

SMRMonRecommendByProfile[tagsArg : _?StringQ, nRes_ : 12, opts : OptionsPattern[]][xs_, context_Association] :=
    SMRMonRecommendByProfile[{tagsArg}, nRes, opts][xs, context];

SMRMonRecommendByProfile[tagsArg : (_?AssociationQ | _List), opts : OptionsPattern[]][xs_, context_Association] :=
    SMRMonRecommendByProfile[tagsArg, 12, opts][xs, context];

SMRMonRecommendByProfile[tagsArg : (_?AssociationQ | _List), nRes : (_Integer | All), opts : OptionsPattern[]][xs_, context_Association] :=
    Block[{tags = tagsArg, p},

      If[ ListQ[tags], tags = AssociationThread[ tags -> 1. ] ];

      If[ TrueQ[OptionValue[SMRMonRecommendByProfile, "IgnoreUnknownTags"]],
        tags = KeyTake[tags, ColumnNames[context["M"]]];
      ];

      If[ AssociationQ[tags] && !ScoredTagsQ[tags, context],
        Echo["The first argument is not an association of tags->score elements or a list of tags.", "SMRMonRecommendByProfile:"];
        Return[$SMRMonFailure]
      ];

      p = SMRMonToProfileVector[tags][xs, context];
      SMRMonRecommendByProfile[p[[1]], nRes, opts][xs, context]
    ];

SMRMonRecommendByProfile[profileVec_SparseArray, nRes : (_Integer | All), opts : OptionsPattern[]][xs_, context_Association] :=
    Block[{itemNamesQ, normalizeQ, vectorResultQ, vec, smat, recs, recsInds, recsVals, ordInds, filterIDs, filterInds, rowNames, fmat},

      itemNamesQ = TrueQ[OptionValue[SMRMonRecommendByProfile, "ItemNames"]];
      normalizeQ = TrueQ[OptionValue[SMRMonRecommendByProfile, "Normalize"]];
      vectorResultQ = TrueQ[OptionValue[SMRMonRecommendByProfile, "VectorResult"]];

      If[!KeyExistsQ[context, "M"],
        Echo["Cannot find the recommendation matrix. (The context key \"M\".)", "SMRMonRecommendByProfile:"];
        Return[$SMRMonFailure]
      ];

      If[ ColumnsCount[context["M"]] != Dimensions[profileVec][[1]],
        Echo[
          "The number of columns of the recommender matrix is different than the length of the profile vector argument.",
          "SMRMonRecommendByProfile:"
        ];
        Return[$SMRMonFailure]
      ];

      smat = SparseArray[context["M"]];

      vec = smat . profileVec ;

      If[ KeyExistsQ[context, "filter"],
        filterIDs = GetFilterIDs[context, "SMRMonRecommendByProfile"];
        rowNames = RowNamesAssociation[context["M"]];
        filterInds = rowNames[#]& /@ filterIDs;
        If[ Length[filterInds] > 0,
          fmat = DiagonalMatrix[ SparseArray[Thread[filterInds -> 1.], RowsCount[context["M"]]] ];
          vec = fmat . vec ;
        ];
      ];

      If[normalizeQ && Max[Abs[vec]] > 0,
        vec = vec / Max[vec];
      ];

      If[ vectorResultQ,

        If[ IntegerQ[ nRes ],
          recsInds = Reverse[ Ordering[ Normal[vec] ] ][[ 1 ;; Min[nRes, Length[vec]] ]];
          vec = SparseArray[ Thread[ recsInds -> vec[[recsInds]] ], RowsCount[context["M"]] ];
        ];

        Return[ SMRMonUnit[ vec, context ] ]
      ];

      (*  recs = Association[ Most[ArrayRules[vec]] ]; *)
      recs = Most[ArrayRules[vec]];

      (*  recs = KeyMap[ First, recs ]; *)
      recsInds = Flatten[recs[[All, 1]]];
      recsVals = recs[[All, 2]];

      ordInds = Reverse[ Ordering[ recsVals ] ];

      If[ IntegerQ[nRes],
        ordInds = Take[ ordInds, UpTo[nRes] ]
      ];

      recsInds = recsInds[[ordInds]];
      recsVals = recsVals[[ordInds]];

      If[itemNamesQ,
        rowNames = RowNames[ context["M"] ];
        recs = AssociationThread[ rowNames[[recsInds]], recsVals ],
        (* ELSE *)
        recs = AssociationThread[ recsInds, recsVals ];
      ];

      SMRMonUnit[recs, context]

    ];

SMRMonRecommendByProfile[___][__] :=
    Block[{},
      Echo[
        "The first argument is expected to be an association of scored tags. " <>
            "The second argument is expected to be a positive integer or All. " <>
            "The tags are recommendation matrix column names. The scores are positive numbers.",
        "SMRMonRecommendByProfile:"];
      $SMRMonFailure
    ];


(**************************************************************)
(* SMRMonRecommendByCorrelation                               *)
(**************************************************************)

Clear[SMRMonRecommendByCorrelation];

SyntaxInformation[SMRMonRecommendByCorrelation] = { "ArgumentsPattern" -> { _, _, OptionsPattern[] } };

Options[SMRMonRecommendByCorrelation] = { Method -> Correlation, "SMRNumberOfRecommendations" -> 200 };

SMRMonRecommendByCorrelation[xs_, context_Association] := $SMRMonFailure;

SMRMonRecommendByCorrelation[ searchVector_?VectorQ, nRes_Integer, opts : OptionsPattern[] ][xs_, context_Association] :=
    Block[{recs, corRecs, smrNRecs, methodFunc, expectedFuncs, corMat},

      methodFunc = OptionValue[ SMRMonRecommendByCorrelation, Method ];

      smrNRecs = OptionValue[ SMRMonRecommendByCorrelation, "SMRNumberOfRecommendations"];
      If[ ! TrueQ[ IntegerQ[smrNRecs] && smrNRecs > 0 ],
        Echo["Positive integer is expected as a value of the option \"SMRNumberOfRecommendations\".", "SMRMonRecommendByCorrelation:"];
        Return[$SMRMonFailure]
      ];

      If[ !KeyExistsQ[context, "timeSeriesMatrix"],
        Echo["Cannot find a time series matrix, context key \"timeSeriesMatrix\".", "SMRMonRecommendByCorrelation:"];
        Return[$SMRMonFailure]
      ];

      recs = Fold[ SMRMonBind, SMRMonUnit[xs, context], { SMRMonRecommendByProfile[ searchVector, smrNRecs ], SMRMonTakeValue }];

      If[ TrueQ[recs === $SMRMonFailure],
        Return[$SMRMonFailure]
      ];

      If[
        ! TrueQ[
          SSparseMatrixQ[context["timeSeriesMatrix"]] &&
              RowNames[context["timeSeriesMatrix"]] == RowNames[context["M"]] &&
              ColumnsCount[context["timeSeriesMatrix"]] == ColumnsCount[context["M"]] ],
        Echo["The value of \"timeSeriesMatrix\" is not a SSparseMatrix object that has the same dimensions and row names as the recommendation matrix."];
        Return[$SMRMonFailure]
      ];

      corMat = context["timeSeriesMatrix"][[Keys[recs], All]];

      (* TO DO: methodFunc is expected to be one of {Correlation, SpearmanRho, KendallTau, Dot} *)
      expectedFuncs = {Correlation, SpearmanRho, KendallTau, Dot};
      If[ !MemberQ[expectedFuncs, methodFunc],
        Echo[ "The value of the option Method is expected to be one of " <> ToString[expectedFuncs] <> "."];
        Return[$SMRMonFailure]
      ];

      corRecs = Flatten @ methodFunc[ Transpose[SparseArray[corMat]], Transpose[{searchVector}] ];

      corRecs = AssociationThread[ RowNames[corMat], corRecs];

      corRecs = TakeLargest[corRecs, UpTo[nRes]];

      SMRMonUnit[corRecs, context]
    ];

SMRMonRecommendByCorrelation[___][__] := $SMRMonFailure;


(**************************************************************)
(* SMRMonBatchRecommend                                       *)
(**************************************************************)

Clear[SMRMonBatchRecommend];

SyntaxInformation[SMRMonBatchRecommend] = { "ArgumentsPattern" -> { _., _., OptionsPattern[] } };

Options[SMRMonBatchRecommend] = {
  "Data" -> None,
  "MatrixResult" -> False,
  "MinScore" -> -Infinity,
  "Normalize" -> True,
  "RemoveHistory" -> True
};

SMRMonBatchRecommend[xs_, context_Association] := $SMRMonFailure;

SMRMonBatchRecommend[xs_, context_Association][] :=
    SMRMonBatchRecommend[ Automatic, 12 ][xs, context];

SMRMonBatchRecommend[xs_, context_Association][ opts : OptionsPattern[] ] :=
    SMRMonBatchRecommend[ Automatic, 12, opts ][xs, context];

SMRMonBatchRecommend[ data : ( Automatic | _List | _?SSparseMatrixQ ), nRes_Integer, opts : OptionsPattern[] ][xs_, context_Association] :=
    Block[{removeHistoryQ, normalizeQ, matrixResultQ, dataRowNames, matRecs, matDiag, aRes},

      If[ ListQ[data] && Length[data] == 0,
        Echo["Empty history as an argument.", "SMRMonBatchRecommend:"];
        Return[SMRMonUnit[None, context]]
      ];

      removeHistoryQ = TrueQ[OptionValue[SMRMonBatchRecommend, "RemoveHistory"]];
      normalizeQ = TrueQ[OptionValue[SMRMonBatchRecommend, "Normalize"]];
      matrixResultQ = TrueQ[OptionValue[SMRMonBatchRecommend, "MatrixResult"]];

      If[!KeyExistsQ[context, "M"],
        Echo["Cannot find the recommendation matrix. (The context key \"M\".)", "SMRMonBatchRecommend:"];
        Return[$SMRMonFailure]
      ];

      dataRowNames = None;
      If[ ListQ[data],

        dataRowNames = Intersection[data, RowNames[context["M"]]];

        If[ Length[dataRowNames] == 0,
          Echo["None of the specified items are known.", "SMRMonBatchRecommend:"];
          Return[SMRMonUnit[None, context]]
        ]

      ];

      If[ !( TrueQ[data === Automatic] || ListQ[dataRowNames] || SSparseMatrixQ[data] && ColumnNames[data] == ColumnNames[context["M"]] ),
        Echo[
          "If the first argument is a SSparseMatrix then its columns are expected to be the same as recommender matrix columns.",
          "SMRMonBatchRecommend:"
        ];
        Return[SMRMonUnit[None, context]]
      ];

      Which[
        TrueQ[ data === Automatic ],
        matRecs = context["M"] . Transpose[ context["M"] ],

        ListQ[ dataRowNames ],
        matRecs = context["M"] . Transpose[ context["M"][[ dataRowNames, All ]] ],

        True,
        matRecs = context["M"] . Transpose[ data ]
      ];

      matRecs = Transpose[matRecs];

      If[ removeHistoryQ && ( TrueQ[data === Automatic] || ListQ[data] ),

        matDiag = DiagonalMatrix[Diagonal[SparseArray[matRecs]]];
        matDiag = ToSSparseMatrix[matDiag, "RowNames" -> RowNames[matRecs], "ColumnNames" -> RowNames[matRecs]];
        matDiag = ImposeColumnNames[matDiag, RowNames[ context["M"] ]];

        matRecs = matRecs - matDiag;
        (* In order to get rid of 0's quickly *)
        matRecs = SSparseMatrix[ReplacePart[matRecs[[1]], "SparseMatrix" -> SparseArray[matRecs[[1]]["SparseMatrix"]]]]
      ];

      (* Unfinished !!! *)
      Which[
        normalizeQ && removeHistoryQ,
        matDiag = DiagonalMatrix[SparseArray[ Normal[Diagonal[SparseArray[matRecs]]] /. {0 -> 1., 0. -> 1.}]];
        matDiag = ToSSparseMatrix[matDiag, "RowNames" -> RowNames[matRecs], "ColumnNames" -> RowNames[matRecs]];
        matRecs = matDiag . matRecs,

        normalizeQ,
        matDiag = DiagonalMatrix[SparseArray[ Normal[Diagonal[SparseArray[matRecs]]] /. {0 -> 1., 0. -> 1.}]];
        matDiag = ToSSparseMatrix[matDiag, "RowNames" -> RowNames[matRecs], "ColumnNames" -> RowNames[matRecs]];
        matRecs = matDiag . matRecs
      ];

      If[ matrixResultQ && !MemberQ[{All, Automatic, Infinity}, nRes],
        Echo["When matrix result all similar items are returned.", "SMRMonBatchRecommend:"]
      ];

      Which[
        matrixResultQ,
        SMRMonUnit[matRecs, context],

        True,
        aRes = TakeLargest[#, UpTo[nRes]] & /@ RowAssociations[matRecs];
        SMRMonUnit[ aRes, context]
      ]
    ];

SMRMonBatchRecommend[___][__] :=
    Block[{},
      Echo[
        "The first argument is expected to be Automatic, a SSparseMatrix object, or a list of recommender items.",
        "SMRMonBatchRecommend:"
      ];
      $SMRMonFailure
    ];


(**************************************************************)
(* Convert recommendations to a Dataset                       *)
(**************************************************************)

Clear[SMRMonToItemsDataset];

SyntaxInformation[SMRMonToItemsDataset] = { "ArgumentsPattern" -> { _. } };

SMRMonToItemsDataset[$SMRMonFailure] := $SMRMonFailure;

SMRMonToItemsDataset[xs_, context_Association] := SMRMonToItemsDataset[xs][xs, context];

SMRMonToItemsDataset[][xs_, context_Association] := SMRMonToItemsDataset[xs][xs, context];

SMRMonToItemsDataset[recs_Association][xs_, context_Association] :=
    Block[{res},

      If[!KeyExistsQ[context, "M"],
        Echo["Cannot find the recommendation matrix. (The context key \"M\".)", "SMRMonToItemsDataset:"];
        Return[$SMRMonFailure]
      ];

      Which[
        ScoredItemIndexesQ[recs, context],
        res = Dataset[Transpose[{ Values[recs], Keys[recs], RowNames[context["M"]][[Keys[recs]]] }]];
        res = res[All, AssociationThread[{"Score", "Index", "Item"} -> #]&];
        SMRMonUnit[res, context],

        Fold[ SMRMonBind, SMRMonUnit[xs, context], {SMRMonScoredItemsQ[recs], SMRMonTakeValue}],
        res = Dataset[Transpose[{ Values[recs], context["itemNames"][#]& /@ Keys[recs], Keys[recs]}]];
        res = res[All, AssociationThread[{"Score", "Index", "Item"} -> #]&];
        SMRMonUnit[res, context],

        True,
        Return[$SMRMonFailure]
      ]
    ];

SMRMonToItemsDataset[__][___] := $SMRMonFailure;


(**************************************************************)
(* Join recommendations with a Dataset                       *)
(**************************************************************)

Clear[SMRMonJoinAcross];

SyntaxInformation[SMRMonJoinAcross] = { "ArgumentsPattern" -> { _., _., OptionsPattern[] } };

Options[SMRMonJoinAcross] = {"DropJoiningColumnName" -> True, "DatasetResult" -> True, "Warning" -> True };

SMRMonJoinAcross[$SMRMonFailure] := $SMRMonFailure;

SMRMonJoinAcross[xs_, context_Association] := $SMRMonFailure;

SMRMonJoinAcross[][xs_, context_Association] := SMRMonJoinAcross[Options[SMRMonJoinAcross]][xs, context];

SMRMonJoinAcross[ds_Dataset, opts : OptionsPattern[]][xs_, context_Association] :=
    Block[{warnQ, byColName, keys},

      warnQ = TrueQ[OptionValue[SMRMonJoinAcross, "Warning"]];

      keys = Normal @ Keys @ ds[[1]];

      Which[
        MemberQ[ ToLowerCase[keys], "id" ],
        byColName = keys[[ First @ Flatten @ Position[ ToLowerCase[keys], "id" ] ]],

        MemberQ[ ToLowerCase[keys], "item" ],
        byColName = keys[[ First @ Flatten @ Position[ ToLowerCase[keys], "item" ] ]],

        True,
        byColName = First @ Normal @ Keys @ ds[[1]];
      ];

      If[ warnQ,
        Echo[ "Heuristically picking the joining column to be \"" <> byColName <> "\".", "SMRMonJoinAcross:" ]
      ];

      SMRMonJoinAcross[ds, byColName, opts][xs, context]
    ];

SMRMonJoinAcross[dsArg_Dataset, byColName_?AtomQ, opts : OptionsPattern[]][xs_, context_Association] :=
    Block[{warnQ, ds = dsArg, dropQ, dsRecs, res},

      warnQ = TrueQ[OptionValue[SMRMonJoinAcross, "Warning"]];

      dropQ = TrueQ[ OptionValue[SMRMonJoinAcross, "DropJoiningColumnName"] ];

      dsRecs = Fold[ SMRMonBind, SMRMonUnit[xs, context], { SMRMonToItemsDataset, SMRMonTakeValue } ];

      If[ TrueQ[dsRecs === $SMRMonFailure],
        Return[$SMRMonFailure]
      ];

      If[ !VectorQ[Normal[ds[All, byColName]], StringQ],
        If[ warnQ,
          Echo["The joining column, \"" <> byColName <> "\", is expected to consist of strings.", "SMRMonJoinAcross:"]
        ];

        If[ VectorQ[Normal[ds[All, byColName]], IntegerQ],
          If[ warnQ,
            Echo["Proceeding by converting the integers in \"" <> byColName <> "\" into strings.", "SMRMonJoinAcross:"]
          ];
          ds = ds[All, Join[#, <| byColName -> ToString[#[byColName]]|>]& ]
          ,
          (*ELSE*)
          Return[$SMRMonFailure]
        ];
      ];

      res = JoinAcross[ Normal[dsRecs[All, {"Score", "Item"}]], Normal[ds], Key["Item"] -> Key[byColName] ];
      res = Dataset[res][SortBy[-#Score &]];

      If[ dropQ, res = res[All, KeyDrop[#, byColName]&] ];

      SMRMonUnit[res, context]
    ];

SMRMonJoinAcross[ asc_Association, opts : OptionsPattern[] ][xs_, context_Association] :=
    SMRMonJoinAcross[  "Value" -> asc, opts ][xs, context];

SMRMonJoinAcross[ dataName_String -> asc_Association, opts : OptionsPattern[]][xs_, context_Association] :=
    Block[{dsRecs, dsQ, datasetQ},

      dsQ = TrueQ[ DatasetWithScoredItemsQ[xs, context] ];

      datasetQ = TrueQ[ OptionValue["DatasetResult"] ] || dsQ ;

      (* The assumption is that the monad value xs is recommendations result. *)
      (* I.e. an association of with item names as keys and real numbers as values. *)
      (* This should be checked and verified. *)

      If[ !( TrueQ[ ScoredItemsQ[xs, context] ] || dsQ ),
        Echo[
          "The pipeline value is not an association of scored items or a Dataset that with scored items.",
          "SMRMonJoinAcross:"
        ];
        Return[$SMRMonFailure];
      ];

      If[ datasetQ,

        If[ dsQ,
          dsRecs = xs,
          dsRecs = Fold[ SMRMonBind, SMRMonUnit[xs, context], { SMRMonToItemsDataset, SMRMonTakeValue } ]
        ];

        If[ TrueQ[dsRecs === $SMRMonFailure],
          Return[$SMRMonFailure]
        ];

        SMRMonUnit[ dsRecs[ All, Join[ #, <| dataName -> asc[#Item] |>]& ], context ],
        (* ELSE *)

        dsRecs =
            KeyValueMap[
              If[ AssociationQ[ asc[#1] ],
                Prepend[ asc[#1], "RecommendationScore" -> #2 ],
                <| "RecommendationScore" -> #2, dataName -> asc[#1] |>
              ]&,
              xs ];

        SMRMonUnit[ dsRecs, context ]
      ]
    ];

SMRMonJoinAcross[opts : OptionsPattern[]][xs_, context_Association] :=
    Block[{warnQ},

      warnQ = TrueQ[OptionValue[SMRMonJoinAcross, "Warning"]];

      If[ KeyExistsQ[context, "data"] && TrueQ[ Head[context["data"]] === Dataset ],
        If[ warnQ,
          Echo[ "Using the dataset for the context key \"data\".", "SMRMonJoinAcross:" ]
        ];
        SMRMonJoinAcross[context["data"], opts][xs, context],
        (*ELSE*)
        SMRMonJoinAcross["NoData"][xs, context]
      ]
    ];

SMRMonJoinAcross[__][___] :=
    Block[{},
      Echo[
        "The first argument is expected to be an Association or a Dataset. " <>
            "If the first argument is a Dataset then the second argument is expected to be a column name to join with. " <>
            "If no arguments are given then context[\"data\"] is used if it exists.",
        "SMRMonJoinAcross:"];
      $SMRMonFailure
    ];


(**************************************************************)
(* SMRMonProfile                                              *)
(**************************************************************)

(* Essentially repeating SMRMonRecommend but with minor changes. *)

Clear[SMRMonProfile];

SyntaxInformation[SMRMonProfile] = { "ArgumentsPattern" -> { _, _., OptionsPattern[] } };

Options[SMRMonProfile] = {"TagNames" -> True, "VectorResult" -> False };

SMRMonProfile[$SMRMonFailure] := $SMRMonFailure;

SMRMonProfile[xs_, context_Association] := $SMRMonFailure;

SMRMonProfile[ itemName_String, opts : OptionsPattern[]][xs_, context_Association] :=
    SMRMonProfile[ AssociationThread[ itemName, 1], opts ][xs, context];

SMRMonProfile[ itemNames : {_String .. }, opts : OptionsPattern[]][xs_, context_Association] :=
    SMRMonProfile[ AssociationThread[itemNames, 1], opts ][xs, context];

SMRMonProfile[ history : Association[ (_String -> _?NumberQ) ... ], opts : OptionsPattern[]][xs_, context_Association] :=
    Block[{h},

      h = KeyMap[ context["itemNames"][#]&, history ];
      h = KeySelect[ h, IntegerQ ];

      If[ 0 == Length[h],
        Echo["None of the item names is known by the recommender.", "SMRMonProfile:"];
        Return[$SMRMonFailure];
      ];

      If[ 0 < Length[h] < Length[history],
        Echo["Some of the item names are not known by the recommender.", "SMRMonProfile:"];
      ];

      SMRMonProfile[ Keys[h], Values[h], opts][xs, context]
    ];

SMRMonProfile[ history : Association[ (_Integer -> _?NumberQ) ... ], opts : OptionsPattern[]][xs_, context_Association] :=
    SMRMonProfile[ Keys[history], Values[history], opts][xs, context];

SMRMonProfile[ itemIndices : {_Integer...}, opts : OptionsPattern[]][xs_, context_Association] :=
    SMRMonProfile[ itemIndices, ConstantArray[1, Length[itemIndices]], opts][xs, context];

SMRMonProfile[ itemIndices : {_Integer...}, itemRatings : {_?NumberQ...}, opts : OptionsPattern[]][xs_, context_Association] :=
    Block[{vec, smat, prof, tagNamesQ, vectorResultQ, columnNames},

      If[Length[itemIndices],
        Echo["Empty history as an argument.", "SMRMonProfile:"];
        Return[<||>]
      ];

      tagNamesQ = TrueQ[OptionValue[SMRMonProfile, "TagNames"]];
      vectorResultQ = TrueQ[OptionValue[SMRMonProfile, "VectorResult"]];

      If[!KeyExistsQ[context, "M"],
        Echo["Cannot find the recommendation matrix. (The context key \"M\".)", "SMRMonProfile:"];
        Return[$SMRMonFailure]
      ];

      smat = SparseArray[context["M"]];

      (*1*)
      vec = SparseArray[Thread[itemIndices -> itemRatings], {Length[smat]}];

      (*2*)
      vec = vec.smat;

      If[ vectorResultQ,
        Return[ SMRMonUnit[vec, context] ]
      ];

      (*3 and 4 and 5*)

      prof = Association[ Most[ArrayRules[vec]] ];

      prof = KeyMap[ First, prof ];

      If[tagNamesQ,
        columnNames = ColumnNames[ context["M"] ];
        prof = KeyMap[ columnNames[[#]]&, prof];
      ];

      SMRMonUnit[prof, context]

    ] /; Length[itemIndices] == Length[itemRatings];


SMRMonProfile[___][__] :=
    Block[{},
      Echo[
        "The first argument is expected to be an association of scored items. " <>
            "The items are recommendation matrix row indices or row names. The scores are positive numbers.",
        "SMRMonProfile:"];
      $SMRMonFailure
    ];


(**************************************************************)
(* SMRMonToProfileVector                                      *)
(**************************************************************)

Clear[SMRMonToProfileVector];

SyntaxInformation[SMRMonToProfileVector] = { "ArgumentsPattern" -> {_} };

SMRMonToProfileVector[$SMRMonFailure] := $SMRMonFailure;

SMRMonToProfileVector[xs_, context_Association] := $SMRMonFailure;

SMRMonToProfileVector[prof : ( {} | <||> ) ][xs_, context_Association] := SMRMonUnit[ 0, context ];

SMRMonToProfileVector[ prof : ( { _String ..} | { _Integer .. } ) ][xs_, context_Association] :=
    SMRMonToProfileVector[ AssociationThread[prof -> 1.] ][xs, context];

SMRMonToProfileVector[ scoredInds : Association[ (_Integer -> _?NumberQ) .. ] ][xs_, context_Association] :=
    Block[{},

      If[ ! KeyExistsQ[context, "M"],
        Echo["Cannot find recommendation matrix, \"M\".", "SMRMonToProfileVector:"];
        Return[$SMRMonFailure]
      ];

      If[ ! Apply[ And, Map[ 0 < # <= ColumnsCount[context["M"]] &, Keys[scoredInds] ] ],
        Echo["Not all index keys are valid recommendation matrix column indices.", "SMRMonToProfileVector:"];
        Return[$SMRMonFailure]
      ];

      SMRMonUnit[ SparseArray[ Normal @ scoredInds, ColumnsCount[context["M"]] ], context]
    ];

SMRMonToProfileVector[ scoredTags : Association[ (_String -> _?NumberQ) .. ] ][xs_, context_Association] :=
    Block[{},

      If[ ! KeyExistsQ[context, "M"],
        Echo["Cannot find recommendation matrix, \"M\".", "SMRMonToProfileVector:"];
        Return[$SMRMonFailure]
      ];

      If[ !ScoredTagsQ[scoredTags, context],
        Echo["Not all tags are valid recommendation matrix column names.", "SMRMonToProfileVector:"];
        Return[$SMRMonFailure]
      ];

      SMRMonToProfileVector[ KeyMap[ ColumnNamesAssociation[context["M"]][#]&, scoredTags] ][xs, context]
    ];

SMRMonToProfileVector[args___][__] :=
    Block[{},
      Echo[ "Called with " <> ToString[{args}], "SMRMonToProfileVector:" ];
      $SMRMonFailure
    ];


(**************************************************************)
(* SMRMonSetTagTypeWeights                                    *)
(**************************************************************)

Clear[SMRMonSetTagTypeWeights];

SyntaxInformation[SMRMonSetTagTypeWeights] = { "ArgumentsPattern" -> { _, _., OptionsPattern[] } };

Options[SMRMonSetTagTypeWeights] = { "CompactAfterColumnBind" -> True };

SMRMonSetTagTypeWeights[$SMRMonFailure] := $SMRMonFailure;

SMRMonSetTagTypeWeights[xs_, context_Association] := $SMRMonFailure;

SMRMonSetTagTypeWeights[ defaultValue_?NumberQ, opts : OptionsPattern[] ][xs_, context_Association] :=
    SMRMonSetTagTypeWeights[ <||>, defaultValue, opts ][xs, context];

SMRMonSetTagTypeWeights[ scoredTagTypes_Association, opts : OptionsPattern[] ][xs_, context_Association] :=
    SMRMonSetTagTypeWeights[ scoredTagTypes, 1., opts ][xs, context];

SMRMonSetTagTypeWeights[ scoredTagTypesArg : Association[ (_String -> _?NumberQ)...], defaultValue_?NumberQ, opts : OptionsPattern[] ][xs_, context_Association] :=
    Block[{ scoredTagTypes = scoredTagTypesArg, compactQ, smats, mat},

      compactQ = TrueQ[ OptionValue[ SMRMonSetTagTypeWeights, "CompactAfterColumnBind" ] ];

      If[!KeyExistsQ[context, "matrices"],
        Echo["Cannot find the recommendation sub-matrices. (The context key \"matrices\".)", "SMRMonSetTagTypeWeights:"];
        Return[$SMRMonFailure]
      ];

      scoredTagTypes = KeySort[ Join[ AssociationThread[ Keys[context["matrices"]] -> defaultValue ], scoredTagTypes ] ];

      (* Optimization -- useful with interactive interfaces. *)
      If[ KeyExistsQ[ context, "TagTypeWeights" ] && context["TagTypeWeights"] == scoredTagTypes,
        Return[ SMRMonUnit[xs, context] ]
      ];

      smats = Association[KeyValueMap[ #1 -> scoredTagTypes[#1] * #2 &, context["matrices"]]];

      mat = ColumnBind[ Values[smats] ];

      If[ compactQ,
        mat = ToSSparseMatrix[ SparseArray[SparseArray[mat]], "RowNames" -> RowNames[mat], "ColumnNames" -> ColumnNames[mat] ]
      ];

      SMRMonUnit[xs, Join[ context, <| "M" -> mat, "TagTypeWeights" -> scoredTagTypes |> ]]

    ];

SMRMonSetTagTypeWeights[___][__] :=
    Block[{},
      Echo[
        "The first argument is expected to be an Association of scored tag-types. " <>
            "The second, optional argument is expected to be a number.",
        "SMRMonSetTagTypeWeights:"];
      $SMRMonFailure
    ];


(**************************************************************)
(* SMRMonSetTagWeights                                        *)
(**************************************************************)

Clear[SMRMonSetTagWeights];

SyntaxInformation[SMRMonSetTagWeights] = { "ArgumentsPattern" -> { _, ___ } };

SMRMonSetTagWeights[$SMRMonFailure] := $SMRMonFailure;

SMRMonSetTagWeights[xs_, context_Association] := $SMRMonFailure;

SMRMonSetTagWeights[ defaultValue_?NumberQ ][xs_, context_Association] :=
    SMRMonSetTagWeights[ <||>, defaultValue][xs, context];

SMRMonSetTagWeights[ scoredTags_Association ][xs_, context_Association] :=
    SMRMonSetTagWeights[ scoredTags, 1][xs, context];

SMRMonSetTagWeights[ scoredTagsArg : Association[ (_String -> _?NumberQ)...], defaultValue_?NumberQ ][xs_, context_Association] :=
    Block[{ scoredTags = scoredTagsArg, mat},

      If[!KeyExistsQ[context, "M"],
        Echo["Cannot find the recommendation matrix. (The context key \"M\".)", "SMRMonSetTagWeights:"];
        Return[$SMRMonFailure]
      ];

      scoredTags = Join[ AssociationThread[ ColumnNames[context["M"]] -> defaultValue ], scoredTags ];

      mat = DiagonalMatrix[ SparseArray[ scoredTags[#]& /@ ColumnNames[context["M"]] ] ];
      mat = ToSSparseMatrix[mat, "RowNames" -> ColumnNames[context["M"]], "ColumnNames" -> ColumnNames[context["M"]] ];

      mat = context["M"] . mat;

      SMRMonUnit[xs, Join[ context, <| "M" -> mat |> ]]

    ];

SMRMonSetTagWeights[___][__] :=
    Block[{},
      Echo[
        "The first argument is expected to be an Association of scored tags." <>
            "The second, optional argument is expected to be a number.",
        "SMRMonSetTagWeights:"];
      $SMRMonFailure
    ];


(**************************************************************)
(* SMR Annex sub matrices                                     *)
(**************************************************************)

Clear[SMRMonAnnexSubMatrices];

SyntaxInformation[SMRMonAnnexSubMatrices] = { "ArgumentsPattern" -> { _, _., OptionsPattern[] } };

Options[SMRMonAnnexSubMatrices] = Options[SMRMonCreate];

SMRMonAnnexSubMatrices[$SMRMonFailure] := $SMRMonFailure;

SMRMonAnnexSubMatrices[xs_, context_Association] := $SMRMonFailure;

SMRMonAnnexSubMatrices[ smat_?SSparseMatrixQ, tagType_String, opts : OptionsPattern[] ][xs_, context_Association] :=
    SMRMonAnnexSubMatrices[ <| tagType -> smat |> ][xs, context];

SMRMonAnnexSubMatrices[ smats : Association[ (_String -> _?SSparseMatrixQ) .. ], opts : OptionsPattern[] ][xs_, context_Association] :=
    Block[{smr2},

      smr2 = SMRMonBind[ SMRMonUnit[], SMRMonCreate[smats, FilterRules[{opts}, Options[SMRMonCreate]]] ];

      SMRMonBind[ SMRMonUnit[xs, context], SMRMonJoin[smr2, "left" ] ]
    ];

SMRMonAnnexSubMatrices[___][__] :=
    Block[{},
      Echo[
        "The expected signatures are SMRMonAnnexSubMatrices[ smat_?SSparseMatrixQ, tagType_String, opts___] or SMRMonAnnexSubMatrices[ smats : <| (_String -> _?SSparseMatrixQ) .. |>, opts___ ].",
        "SMRMonAnnexSubMatrices:"];
      $SMRMonFailure
    ];


Clear[SMRMonAnnexSubMatrix];
SMRMonAnnexSubMatrix = SMRMonAnnexSubMatrices;


(**************************************************************)
(* SMR Join                                                   *)
(**************************************************************)

Clear[SMRMonJoin];

SyntaxInformation[SMRMonJoin] = { "ArgumentsPattern" -> { _, _., OptionsPattern[] } };

Options[SMRMonJoin] =
    Join[
      { "JoinType" -> Automatic },
      Options[SMRMonCreate]
    ];

SMRMonJoin[$SMRMonFailure] := $SMRMonFailure;

SMRMonJoin[xs_, context_Association] := $SMRMonFailure;

SMRMonJoin[ smr2_SMRMon, opts : OptionsPattern[] ][xs_, context_Association] :=
    Block[{joinType, lsExpectedJoinTypes},

      lsExpectedJoinTypes = {"same", "outer", "union", "inner", "left"};

      joinType = OptionValue[SMRMonJoin, "JoinType"];
      joinType = If[ joinType === Automatic, "same", joinType];
      If[ !( StringQ[joinType] && MemberQ[lsExpectedJoinTypes, joinType] ),
        Echo["The value of the option \"JoinType\" is expected to be Automatic or one of : \"" <> StringRiffle[lsExpectedJoinTypes, "\", \""] <> "\"."];
        Return[$SMRMonFailure];
      ];

      SMRMonJoin[ smr2, "JoinType" -> joinType, opts ][xs, context]
    ];

SMRMonJoin[ smr2_SMRMon, joinType_String, opts : OptionsPattern[] ][xs_, context_Association] :=
    SMRMonJoin[ smr2, joinType, opts][xs, context];

SMRMonJoin[
  smr2_SMRMon,
  joinTypeArg_String,
  opts : OptionsPattern[] ][xs_, context_Association] :=
    Block[{joinType = ToLowerCase[joinTypeArg], smats1, smats2, allRownames, matrices},

      (*  Get the appropriate all row names. *)
      If[ joinType != "same",

        Which[
          MemberQ[ {"outer", "union" }, joinType],
          allRownames =
              Union[ Flatten[ {
                RowNames[ context["M"] ],
                RowNames[ SMRMonBind[ smr2, SMRMonTakeContext]["M"] ]
              }] ],

          joinType == "inner",
          allRownames =
              Intersection[
                RowNames[ context["M"] ],
                RowNames[ SMRMonBind[ smr2, SMRMonTakeContext]["M"] ] ],

          joinType == "left",
          allRownames = RowNames[ context["M"] ],

          True,
          Echo[ "The second argument is expected to be one of \"same\", \"outer\", \"inner\", \"left\".", "SMRMonJoin:"];
          Return[$SMRMonFailure];

        ];

      ];

      smats1 = context["matrices"];

      smats2 = SMRMonBind[ smr2, SMRMonTakeContext]["matrices"];

      If[ Length[ Intersection[ Keys[smats1], Keys[smats2] ] ] > 0,
        Echo[
          "The tag types " <> ToString[ Intersection[ Keys[smats1], Keys[smats2] ] ] <>
              " are also in the SMRMon argument, hence will be dropped.",
          "SMRMonJoin:"
        ];
      ];

      matrices =
          Join[
            If[joinType == "same", smats1, Map[ ImposeRowNames[#, allRownames]&, smats1 ]],
            If[joinType == "same", smats2, Map[ ImposeRowNames[#, allRownames]&, smats2 ]]
          ];

      Fold[ SMRMonBind, SMRMonUnit[], {SMRMonCreate[ matrices, FilterRules[{opts}, Options[SMRMonJoin]] ] } ]
    ];

SMRMonJoin[___][__] :=
    Block[{},
      Echo[
        "The first argument is expected to be a SMRMon object. " <>
            "The second argument is expected to be a string specifying the type of joining.",
        "SMRMonJoin:"];
      $SMRMonFailure
    ];


(**************************************************************)
(* SMR row-bind                                               *)
(**************************************************************)

Clear[SMRMonRowBind];

SyntaxInformation[SMRMonRowBind] = { "ArgumentsPattern" -> {_} };

SMRMonRowBind[$SMRMonFailure] := $SMRMonFailure;

SMRMonRowBind[xs_, context_Association] := $SMRMonFailure;

SMRMonRowBind[smr2_SMRMon][xs_, context_Association] :=
    Block[{smats1, smats2, resSMmats, cnames},

      smats1 = context["matrices"];

      smats2 = SMRMonBind[ smr2, SMRMonTakeMatrices ];

      Which[

        Keys[smats1] == Keys[smats2],

        (* Row bind each pair. *)
        resSMmats =
            MapThread[
              ( cnames = Union[Join[ColumnNames[#1], ColumnNames[#2]]];
              RowBind[ ImposeColumnNames[#1, cnames], ImposeColumnNames[#2, cnames] ])&,
              {smats1, smats2}];


        SMRMonCreate[resSMmats][xs, context],

        True,
        Echo["The tag types of the SMRMon objects to be joined (row-bound) are not the same.", "SMRMonJoin:"];
        $SMRMonFailure
      ]

    ];

SMRMonRowBind[___][__] :=
    Block[{},
      Echo[ "The first argument is expected to be a SMRMon object.", "SMRMonRowBind:"];
      $SMRMonFailure
    ];


(*=========================================================*)
(* Classify (original implementation)                      *)
(*=========================================================*)

Clear[SMRMonClassifyOriginal];

Options[SMRMonClassifyOriginal] = {
  "TagType" -> None, "Profile" -> None, "Property" -> "Probabilities",
  "NumberOfNearestNeighbors" -> Automatic, "NumberOfResults" -> All,
  "Normalize" -> True, "Voting" -> False, "DropZeroScoredLabels" -> True
};

SMRMonClassifyOriginal[$SMRMonFailure] := $SMRMonFailure;

(*SMRMonClassifyOriginal[xs_, context_Association] := (Print["here"];$SMRMonFailure);*)

SMRMonClassifyOriginal[][xs_, context_Association] := SMRMonClassifyOriginal[None][xs, context];

SMRMonClassifyOriginal[ opts : OptionsPattern[] ][xs_, context_Association] :=
    Block[{tagType, profile},

      tagType = OptionValue[SMRMonClassifyOriginal, "TagType"];
      profile = OptionValue[SMRMonClassifyOriginal, "Profile"];

      If[ ! StringQ[ tagType ],
        Echo["The value of the option \"TagType\" is expected to be a string.", "SMRMonClassifyOriginal:"];
        Return[$SMRMonFailure]
      ];

      If[ ! ( AssociationQ[ profile ] || VectorQ[ profile, StringQ ] ),
        Echo["The value of the option \"Profile\" is expected to be an association or a list of strings.", "SMRMonClassifyOriginal:"];
        Return[$SMRMonFailure]
      ];

      SMRMonClassifyOriginal[tagType, profile, opts][xs, context]
    ];

SMRMonClassifyOriginal[tagType_String, profile : {_String..}, opts : OptionsPattern[]][xs_, context_Association] :=
    SMRMonClassifyOriginal[tagType, AssociationThread[profile, 1], opts][xs, context];

SMRMonClassifyOriginal[tagType_String, profile_Association, opts : OptionsPattern[]][xs_, context_Association] :=
    Block[{recs, clMat, clMat01, s, t,
      property, expectedProperties, numberOfNearestNeighbors, numberOfResults,
      normalizeQ, votingQ, dropZeroScoredLabelsQ, qProfile, res, recsVec},

      property = OptionValue[SMRMonClassifyOriginal, "Property"];
      numberOfNearestNeighbors = OptionValue[SMRMonClassifyOriginal, "NumberOfNearestNeighbors"];
      numberOfResults = OptionValue[SMRMonClassifyOriginal, "NumberOfResults"];

      normalizeQ = TrueQ[OptionValue[SMRMonClassifyOriginal, "Normalize"]];
      votingQ = TrueQ[OptionValue[SMRMonClassifyOriginal, "Voting"]];
      dropZeroScoredLabelsQ = TrueQ[OptionValue[SMRMonClassifyOriginal, "DropZeroScoredLabels"]];

      expectedProperties = { "Decision", "Probabilities", "Properties" };
      If[ ! MemberQ[ expectedProperties, property ],
        Echo["The value of the option \"Property\" is expected to be one of " <> ToString[expectedProperties] <> ".", "SMRMonClassifyOriginal:"];
        Return[$SMRMonFailure]
      ];

      If[ ! ( TrueQ[numberOfNearestNeighbors === Automatic] || NumberQ[ numberOfNearestNeighbors ] ),
        Echo["The value of the option \"NumberOfNearestNeighbors\" is expected to be a number or Automatic.", "SMRMonClassifyOriginal:"];
        Return[$SMRMonFailure]
      ];

      If[ ! ( TrueQ[numberOfResults === All] || NumberQ[ numberOfResults ] ),
        Echo["The value of the option \"NumberOfResults\" is expected to be a number or All.", "SMRMonClassifyOriginal:"];
        Return[$SMRMonFailure]
      ];

      If[ ! KeyExistsQ[context, "matrices"],
        Echo["Cannot find the recommendation sub-matrices. (The context key \"matrices\".)", "SMRMonClassifyOriginal:"];
        Return[$SMRMonFailure]
      ];

      If[ !MemberQ[ Keys[context["matrices"]], tagType],
        Echo[
          "Unknown tag type \"" <> tagType <> "\"; the first argument should be one of: " <>
              ToString[ "\"" <> # <> "\""& /@ Keys[context["matrices"]] ] <> " .",
          "SMRMonClassifyOriginal:"
        ];
        Return[$SMRMonFailure]
      ];

      clMat = context["matrices", tagType];

      qProfile = KeySelect[ profile, !MemberQ[ColumnNames[clMat], #]& ];

      If[ Length[qProfile] == 0,
        Echo[
          "The profile argument has to have at least one tag that does not belong to the tag type \"" <> tagType <> "\".",
          "SMRMonClassifyOriginal:"
        ];
        Return[$SMRMonFailure]
      ];

      If[ TrueQ[ numberOfNearestNeighbors === Automatic ],
        numberOfNearestNeighbors = Ceiling[ 0.05 * RowsCount[ clMat ] ];
        numberOfNearestNeighbors = If[ numberOfNearestNeighbors < 10, 10, numberOfNearestNeighbors ];
        numberOfNearestNeighbors = If[ numberOfNearestNeighbors > 200, 200, numberOfNearestNeighbors ];
        numberOfNearestNeighbors = Min[ numberOfNearestNeighbors, RowsCount[clMat] ];
      ];

      recs = Fold[ SMRMonBind, SMRMonUnit[xs, context], {SMRMonRecommendByProfile[qProfile, numberOfNearestNeighbors], SMRMonTakeValue}];

      If[ TrueQ[recs === $SMRMonFailure],
        Return[$SMRMonFailure]
      ];

      If[ votingQ,
        clMat01 = SparseArray[clMat];
        t = Most[ArrayRules[clMat]]; t[[All, 2]] = 1.;
        clMat01 = SparseArray[t, Dimensions[clMat]];
        clMat = ToSSparseMatrix[ clMat01, "RowNames" -> RowNames[clMat], "ColumnNames" -> ColumnNames[clMat] ];
        recs = AssociationThread[ Keys[recs], 1.];
      ];

      (* Finally the "classification" computation follows. *)
      s = If[ normalizeQ, ( Values[recs] / Max[recs] ), Values[recs]];
      s = s . SparseArray[ clMat[[ Keys[recs], All ]] ];

      If[ Max[Abs[s]] > 0, s = s / Max[s] ];

      s = AssociationThread[ ColumnNames[clMat], s];

      If[ dropZeroScoredLabelsQ, s = Select[s, # > 0&] ];

      res = ReverseSort[s];

      res =
          Which[
            property == "Decision", First[Keys[res]],

            property == "Probabilities" && TrueQ[ numberOfResults === All ], res,

            property == "Probabilities", TakeLargest[ res, UpTo[numberOfResults] ],

            property == "Properties", expectedProperties,

            True, res
          ];

      SMRMonUnit[ res, context ]
    ];

SMRMonClassifyOriginal[___][__] :=
    Block[{},
      Echo[
        "The expected signature is SMRMonClassifyOriginal[tagType_String, profile_Association, opts___].",
        "SMRMonClassifyOriginal:"];
      $SMRMonFailure
    ];


(*=========================================================*)
(* Classify                                                *)
(*=========================================================*)

Clear[SMRMonClassify];

SyntaxInformation[SMRMonClassify] = { "ArgumentsPattern" -> {___, ___, OptionsPattern[] } };

Options[SMRMonClassify] = {
  "TagType" -> None, "Profile" -> None, "Property" -> "Probabilities",
  "NumberOfNearestNeighbors" -> Automatic, "NumberOfResults" -> All,
  "Normalize" -> True, "Voting" -> False, "DropZeroScoredLabels" -> True
};

SMRMonClassify[$SMRMonFailure] := $SMRMonFailure;

(*SMRMonClassifyOriginal[xs_, context_Association] := (Print["here"];$SMRMonFailure);*)

SMRMonClassify[][xs_, context_Association] := SMRMonClassify[None][xs, context];

SMRMonClassify[ opts : OptionsPattern[] ][xs_, context_Association] :=
    Block[{tagType, profile},

      tagType = OptionValue[SMRMonClassify, "TagType"];
      profile = OptionValue[SMRMonClassify, "Profile"];

      If[ ! StringQ[ tagType ],
        Echo["The value of the option \"TagType\" is expected to be a string.", "SMRMonClassifyOriginal:"];
        Return[$SMRMonFailure]
      ];

      If[ ! ( AssociationQ[ profile ] || VectorQ[ profile, StringQ ] ),
        Echo["The value of the option \"Profile\" is expected to be an association or a list of strings.", "SMRMonClassifyOriginal:"];
        Return[$SMRMonFailure]
      ];

      SMRMonClassify[tagType, profile, opts][xs, context]
    ];

SMRMonClassify[tagType_String, profile : {_String..}, opts : OptionsPattern[]][xs_, context_Association] :=
    SMRMonClassify[tagType, AssociationThread[profile, 1], opts][xs, context];

SMRMonClassify[tagType_String, profile_Association, opts : OptionsPattern[]][xs_, context_Association] :=
    Block[{recs, clMat, clMat01, cnAsc, s, t,
      property, expectedProperties, numberOfNearestNeighbors, numberOfResults,
      normalizeQ, votingQ, dropZeroScoredLabelsQ, qProfile, resInds, res},

      property = OptionValue[SMRMonClassify, "Property"];
      numberOfNearestNeighbors = OptionValue[SMRMonClassify, "NumberOfNearestNeighbors"];
      numberOfResults = OptionValue[SMRMonClassify, "NumberOfResults"];

      normalizeQ = TrueQ[OptionValue[SMRMonClassify, "Normalize"]];
      votingQ = TrueQ[OptionValue[SMRMonClassify, "Voting"]];
      dropZeroScoredLabelsQ = TrueQ[OptionValue[SMRMonClassify, "DropZeroScoredLabels"]];

      expectedProperties = { "Decision", "Probabilities", "Properties" };
      If[ ! MemberQ[ expectedProperties, property ],
        Echo["The value of the option \"Property\" is expected to be one of " <> ToString[expectedProperties] <> ".", "SMRMonClassify:"];
        Return[$SMRMonFailure]
      ];

      If[ ! ( TrueQ[numberOfNearestNeighbors === Automatic] || NumberQ[ numberOfNearestNeighbors ] ),
        Echo["The value of the option \"NumberOfNearestNeighbors\" is expected to be a number or Automatic.", "SMRMonClassify:"];
        Return[$SMRMonFailure]
      ];

      If[ ! ( TrueQ[numberOfResults === All] || NumberQ[ numberOfResults ] ),
        Echo["The value of the option \"NumberOfResults\" is expected to be a number or All.", "SMRMonClassify:"];
        Return[$SMRMonFailure]
      ];

      If[ ! KeyExistsQ[context, "matrices"],
        Echo["Cannot find the recommendation sub-matrices. (The context key \"matrices\".)", "SMRMonClassify:"];
        Return[$SMRMonFailure]
      ];

      If[ !MemberQ[ Keys[context["matrices"]], tagType],
        Echo[
          "Unknown tag type \"" <> tagType <> "\"; the first argument should be one of: " <>
              ToString[ "\"" <> # <> "\""& /@ Keys[context["matrices"]] ] <> " .",
          "SMRMonClassify:"
        ];
        Return[$SMRMonFailure]
      ];

      clMat = context["matrices", tagType];

      cnAsc = ColumnNamesAssociation[clMat];
      qProfile = KeyComplement[ {profile, cnAsc} ];

      If[ Length[qProfile] == 0,
        Echo[
          "The profile argument has to have at least one tag that does not belong to the tag type \"" <> tagType <> "\".",
          "SMRMonClassify:"
        ];
        Return[$SMRMonFailure]
      ];

      If[ TrueQ[ numberOfNearestNeighbors === Automatic ],
        numberOfNearestNeighbors = Ceiling[ 0.05 * RowsCount[ clMat ] ];
        numberOfNearestNeighbors = If[ numberOfNearestNeighbors < 10, 10, numberOfNearestNeighbors ];
        numberOfNearestNeighbors = If[ numberOfNearestNeighbors > 200, 200, numberOfNearestNeighbors ];
        numberOfNearestNeighbors = Min[ numberOfNearestNeighbors, RowsCount[clMat] ];
      ];

      recs = Fold[ SMRMonBind, SMRMonUnit[xs, context], {SMRMonRecommendByProfile[qProfile, numberOfNearestNeighbors, "VectorResult" -> True ], SMRMonTakeValue}];

      If[ TrueQ[recs === $SMRMonFailure],
        Return[$SMRMonFailure]
      ];

      If[ Total[recs] == 0,
        (* This can happen if the columns corresponding to the given profile tags have only 0's. *)
        res = <| Indeterminate -> 1 |>,
        (* ELSE  *)

        If[ votingQ,
          clMat01 = SparseArray[clMat];
          t = Most[ArrayRules[clMat]]; t[[All, 2]] = 1.;
          clMat01 = SparseArray[t, Dimensions[clMat]];
          clMat = ToSSparseMatrix[ clMat01, "RowNames" -> RowNames[clMat], "ColumnNames" -> ColumnNames[clMat] ];
          recs = SparseArray[ Thread[ Flatten[First /@ Most[ArrayRules[recs]]] -> 1. ], Length[recs] ];
        ];

        (* Finally the "classification" computation follows. *)
        s = recs . SparseArray[ clMat ];

        If[ Max[s] > 0 && normalizeQ, s = s / Max[s] ];

        If[ dropZeroScoredLabelsQ,
          resInds = Range[Length[s]];
          resInds = Pick[resInds, UnitStep[-s], 0];
          resInds = resInds[[ Reverse[ Ordering[ Normal @ s[[resInds]] ] ] ]],

          (* ELSE  *)
          resInds = Reverse[ Ordering[ Normal[s] ] ]
        ];

        res = AssociationThread[ ColumnNames[clMat][[resInds]], Normal[s[[resInds]]] ];
      ];

      res =
          Which[
            property == "Decision", First[Keys[res]],

            property == "Probabilities" && TrueQ[ numberOfResults === All ], res,

            property == "Probabilities", TakeLargest[ res, UpTo[numberOfResults] ],

            property == "Properties", expectedProperties,

            True, res
          ];

      SMRMonUnit[ res, context ]
    ];

SMRMonClassify[___][__] :=
    Block[{},
      Echo[
        "The expected signature is SMRMonClassify[tagType_String, profile_Association, opts___].",
        "SMRMonClassify:"];
      $SMRMonFailure
    ];


(*=========================================================*)
(* Metadata Proofs                                         *)
(*=========================================================*)

Clear[SMRMonProveByMetadata];

SyntaxInformation[SMRMonProveByMetadata] = { "ArgumentsPattern" -> {_, _., OptionsPattern[] } };

Options[SMRMonProveByMetadata] = { "Profile" -> None, "Items" -> None, "OutlierIdentifierParameters" -> None, "Normalize" -> True };

SMRMonProveByMetadata[$SMRMonFailure] := $SMRMonFailure;

SMRMonProveByMetadata[xs_, context_Association] := $SMRMonFailure;

SMRMonProveByMetadata[][xs_, context_Association] := $SMRMonFailure;

SMRMonProveByMetadata[ opts : OptionsPattern[] ][ xs_, context_ ] :=
    Block[{profile, items},

      profile = OptionValue[SMRMonProveByMetadata, "Profile"];
      items = OptionValue[SMRMonProveByMetadata, "Items"];

      SMRMonProveByMetadata[ profile, items, opts][xs, context]
    ];

SMRMonProveByMetadata[ profile_List, items : ( _String | {_String..} ), args___ ][ xs_, context_ ] :=
    SMRMonProveByMetadata[ AssociationThread[profile, 1.], items, args][xs, context];

SMRMonProveByMetadata[ profile_Association, itemName_String, opts : OptionsPattern[] ][ xs_, context_ ] :=
    Block[{scores, oiFunc, normalizeQ, res},

      oiFunc = OptionValue[SMRMonProveByMetadata, "OutlierIdentifierParameters"];
      normalizeQ = TrueQ[ OptionValue[SMRMonProveByMetadata, "Normalize"] ];

      (* Check that the argument profile is a profile according to the monad object. *)
      If[ ! ScoredTagsQ[ profile, context ],
        Echo[ "The first argument is not a profile (an association of scored tags) in the monad object.", "SMRMonProveByMetadata:"];
        Return[$SMRMonFailure]
      ];

      scores = ColumnSumsAssociation[ context["M"][[ {itemName}, Keys[profile] ]] ];
      scores = Select[scores, # > 0 &];

      scores = KeySelect[ profile, MemberQ[Keys[scores], #]& ];

      res =
          If[TrueQ[oiFunc === None],
            scores,
            scores[[ If[# === {}, All, #]& @ OutlierPosition[ Values[scores], TopOutliers @* oiFunc ] ]]
          ];

      If[ normalizeQ && Length[res] > 0,
        res = N[ res / Max[res] ]
      ];

      SMRMonUnit[ <|itemName -> ReverseSort[res] |>, context ]
    ];

SMRMonProveByMetadata[ profile_Association, itemNames : {_String..}, opts : OptionsPattern[] ][ xs_, context_ ] :=
    Block[{res},

      res = Map[ Fold[ SMRMonBind, SMRMonUnit[xs, context], { SMRMonProveByMetadata[ profile, #, opts ], SMRMonTakeValue } ]& , itemNames ];

      If[ !FreeQ[res, $SMRMonFailure],
        Return[$SMRMonFailure]
      ];

      SMRMonUnit[ Join @@ res, context ]
    ];

SMRMonProveByMetadata[ Automatic, itemNames : ( _String | {_String..} ), opts : OptionsPattern[] ][ xs_, context_ ] :=
    SMRMonProveByMetadata[ itemNames, opts ][xs, context];

SMRMonProveByMetadata[ itemNames : ( _String | {_String..} ), opts : OptionsPattern[] ][ xs_, context_ ] :=
    Block[{},
      If[ ScoredTagsQ[xs, context],
        SMRMonProveByMetadata[xs, itemNames, opts][xs, context],
        (* ELSE *)
        SMRMonProveByMetadata[None][xs, context]
      ]
    ];

SMRMonProveByMetadata[___][__] :=
    Block[{},
      Echo[
        "The expected signature is SMRMonProveByMetadata[profile : (_Association | _List | Automatic), itemNames:( _String | {_String..} ), opts___]. " <>
            "If profile is Automatic then the pipeline value is expected to be a valid profile.",
        "SMRMonProveByMetadata:"];
      $SMRMonFailure
    ];



(*=========================================================*)
(* History  Proofs                                         *)
(*=========================================================*)

Clear[SMRMonProveByHistory];

SyntaxInformation[SMRMonProveByHistory] = { "ArgumentsPattern" -> {_, _., OptionsPattern[] } };

Options[SMRMonProveByHistory] = { "History" -> None, "Items" -> None, "OutlierIdentifierParameters" -> None, "Normalize" -> True };

SMRMonProveByHistory[$SMRMonFailure] := $SMRMonFailure;

SMRMonProveByHistory[xs_, context_Association] := $SMRMonFailure;

SMRMonProveByHistory[][xs_, context_Association] := $SMRMonFailure;

SMRMonProveByHistory[ opts : OptionsPattern[] ][ xs_, context_ ] :=
    Block[{history, items},

      history = OptionValue[SMRMonProveByHistory, "History"];
      items = OptionValue[SMRMonProveByHistory, "Items"];

      SMRMonProveByHistory[ history, items, opts][xs, context]
    ];

SMRMonProveByHistory[ history_List, items : ( _String | {_String..} ), args___ ][ xs_, context_ ] :=
    SMRMonProveByHistory[ AssociationThread[history, 1.], items, args][xs, context];

SMRMonProveByHistory[ history_Association, itemName_String, opts : OptionsPattern[] ][ xs_, context_ ] :=
    Block[{ oiFunc, normalizeQ, scores, maxScore, res},

      oiFunc = OptionValue[SMRMonProveByMetadata, "OutlierIdentifierParameters"];
      normalizeQ = TrueQ[ OptionValue[SMRMonProveByMetadata, "Normalize"] ];

      (* Check that the argument profile is a profile according to the monad object. *)
      If[ ! ScoredItemsQ[ history, context ],
        Echo[ "The first argument is not an association of scored items in the monad object.", "SMRMonProveByHistory:"];
        Return[$SMRMonFailure]
      ];

      scores = SparseArray[ context["M"][[ Keys[history], All ]] . context["M"][[ itemName, All ]] ];

      If[ normalizeQ && Length[scores] > 0,
        maxScore = context["M"][[ itemName, All ]] . context["M"][[ itemName, All ]];
        scores = N[ scores / maxScore ]
      ];

      scores = AssociationThread[ Keys[history], Normal[SparseArray[scores]] * Values[history] ];

      scores = Select[scores, # > 0 &];

      res =
          If[TrueQ[oiFunc === None],
            scores,
            scores[[ If[# === {}, All, #]& @ OutlierPosition[ Values[scores], TopOutliers @* oiFunc ] ]]
          ];

      If[ normalizeQ && Length[res] > 0,
        res = N[ res / Max[res] ]
      ];

      SMRMonUnit[ <| itemName -> ReverseSort[res] |>, context ]
    ];

SMRMonProveByHistory[ history_Association, itemNames : {_String..}, opts : OptionsPattern[] ][ xs_, context_ ] :=
    Block[{res},

      res = Map[ Fold[ SMRMonBind, SMRMonUnit[xs, context], { SMRMonProveByHistory[ history, #, opts ], SMRMonTakeValue } ]& , itemNames ];

      If[ !FreeQ[res, $SMRMonFailure],
        Return[$SMRMonFailure]
      ];

      SMRMonUnit[ Join @@ res, context ]
    ];

SMRMonProveByHistory[ Automatic, itemNames : ( _String | {_String..} ), opts : OptionsPattern[] ][ xs_, context_ ] :=
    SMRMonProveByHistory[ itemNames, opts ][xs, context];

SMRMonProveByHistory[ itemNames : ( _String | {_String..} ), opts : OptionsPattern[] ][ xs_, context_ ] :=
    Block[{},
      If[ ScoredItemsQ[xs, context],
        SMRMonProveByHistory[xs, itemNames, opts][xs, context],
        (* ELSE *)
        SMRMonProveByHistory[None][xs, context]
      ]
    ];

SMRMonProveByHistory[___][__] :=
    Block[{},
      Echo[
        "The expected signature is SMRMonProveByHistory[history : (_Association | _List | Automatic), itemNames:( _String | {_String..} ), opts___]. " <>
            "If history is Automatic then the pipeline value is expected to be a valid history specification.",
        "SMRMonProveByHistory:"];
      $SMRMonFailure
    ];


(*=========================================================*)
(* Remove tag type                                         *)
(*=========================================================*)

Clear[SMRMonRemoveTagTypes];

SyntaxInformation[SMRMonRemoveTagTypes] = { "ArgumentsPattern" -> {_, OptionsPattern[] } };

Options[SMRMonRemoveTagTypes] = { "TagTypes" -> None };

SMRMonRemoveTagTypes[$SMRMonFailure] := $SMRMonFailure;

SMRMonRemoveTagTypes[xs_, context_Association] := $SMRMonFailure;

SMRMonRemoveTagTypes[][xs_, context_Association] := $SMRMonFailure;

SMRMonRemoveTagTypes[ opts : OptionsPattern[] ][ xs_, context_ ] :=
    Block[{tagTypes},

      tagTypes = OptionValue[SMRMonRemoveTagTypes, "TagTypes"];

      SMRMonRemoveTagTypes[ tagTypes, opts][xs, context]
    ];

SMRMonRemoveTagTypes[ tagTypes : { _String ..}, opts : OptionsPattern[] ][ xs_, context_ ] :=
    Block[{smats},

      smats = SMRMonBind[ SMRMonUnit[xs, context], SMRMonTakeMatrices ];

      If[ Length[Intersection[Keys[smats], tagTypes]] == 0,
        Echo["None of the specified tag types is a known tag type in the recommender object.", "SMRMonRemoveTagTypes:"];
        Return[SMRMonUnit[xs, context]]
      ];

      Fold[ SMRMonBind, SMRMonUnit[], { SMRMonCreate[ KeyDrop[smats, tagTypes] ] }]
    ];

SMRMonRemoveTagTypes[___][__] :=
    Block[{},
      Echo[
        "The expected signature is SMRMonRemoveTagTypes[tagTypes : { _String..}, opts___] .",
        "SMRMonRemoveTagTypes:"];
      $SMRMonFailure
    ];


(*=========================================================*)
(* Compute Top-K statistic                                 *)
(*=========================================================*)

Clear[SMRMonComputeTopK];

SyntaxInformation[SMRMonComputeTopK] = { "ArgumentsPattern" -> {_, _, OptionsPattern[] } };

Options[SMRMonComputeTopK] = { "Type" -> "Fraction" };

SMRMonComputeTopK[$SMRMonFailure] := $SMRMonFailure;

SMRMonComputeTopK[xs_, context_Association] := $SMRMonFailure;

SMRMonComputeTopK[][xs_, context_Association] := $SMRMonFailure;

SMRMonComputeTopK[ testData_Association, ksArg : {_?IntegerQ..}, opts : OptionsPattern[] ][ xs_, context_ ] :=
    Block[{ ks = ksArg, type, expectedTypes, aTopK, recs, topKStat, expectedRecs },

      type = OptionValue[SMRMonComputeTopK, "Type"];

      If[ ! ( MatchQ[ testData, Association[ ( ( _String | _List | _Association ) -> ( _String | {_String..} ) ) .. ] ] ),
        Echo[ "The first argument is expected to be an association that maps profiles or items into expected results.", "SMRMonComputeTopK:"];
        Return[$SMRMonFailure]
      ];

      If[ ! ( VectorQ[ ks, NumberQ ] && Apply[ And, Map[ # > 0&, ks] ] ),
        Echo[ "The second argument is expected to be list of positive integers.", "SMRMonComputeTopK:"];
        Return[$SMRMonFailure]
      ];

      expectedTypes = { "Fraction", "Count", "Incidence", "Binary" };
      If[ ! ( StringQ[type] && MemberQ[ ToLowerCase[expectedTypes], ToLowerCase[type] ] ),
        Echo[ "The value of the option \"Type\" is expected to be one of:" <> ToString[expectedTypes[[1 ;; -2]]] <> "." , "SMRMonComputeTopK:"];
        Return[$SMRMonFailure]
      ];

      ks = Sort[ks];
      type = ToLowerCase[type];

      aTopK =
          Map[
            Function[{search},

              (* Recommendations *)
              recs = Fold[ SMRMonBind, SMRMonUnit[xs, context], { SMRMonGetTopRecommendations[search, Max[ks] ], SMRMonTakeValue } ];

              (* Top-K of the key over the specified ks. *)

              expectedRecs = Flatten[{testData[search]}];

              Which[

                MemberQ[ { "binary", "incidence" }, type ],
                topKStat = Map[ # -> Length[Intersection[ Take[Keys[recs], UpTo[#] ], expectedRecs ]] > 0&, ks],

                type == "count",
                topKStat = Map[ # -> Length[Intersection[ Take[Keys[recs], UpTo[#] ]], expectedRecs ]&, ks],

                type == "fraction",
                topKStat = Map[ # -> Length[Intersection[ Take[Keys[recs], UpTo[#] ], expectedRecs ]] / Length[expectedRecs] &, ks],

                True,
                (* Should not happen. *)
                Return[$SMRMonFailure]
              ];

              topKStat

            ],
            Keys[testData]
          ];

      aTopK = GroupBy[ Join @@ aTopK, First, #[[All, 2]]& ];

      SMRMonUnit[ aTopK, context ]

    ];

SMRMonComputeTopK[___][__] :=
    Block[{},
      Echo[
        "The expected signature is SMRMonComputeTopK[ testData : Association[ ( ( _String | _List | _Association) -> ( _String, {_String..} ) ) .. ], ks : {_?IntegerQ..}, opts___] .",
        "SMRMonComputeTopK:"];
      $SMRMonFailure
    ];


(*=========================================================*)
(* SMRMonToMetadataRecommender                             *)
(*=========================================================*)

Clear[SMRMonToMetadataRecommender];

SyntaxInformation[SMRMonToMetadataRecommender] = { "ArgumentsPattern" -> {_., OptionsPattern[] } };

Options[SMRMonToMetadataRecommender] =
    Join[
      { "TagTypeTo" -> None, "NumberOfTopTags" -> 1, "TagTypesFrom" -> All, "TagSelectionCriteria" -> Automatic},
      Options[SMRMonCreate]
    ];

SMRMonToMetadataRecommender[$SMRMonFailure] := $SMRMonFailure;

SMRMonToMetadataRecommender[xs_, context_Association] := $SMRMonFailure;

SMRMonToMetadataRecommender[][xs_, context_Association] := $SMRMonFailure;

SMRMonToMetadataRecommender[ opts : OptionsPattern[] ][ xs_, context_ ] :=
    Block[{ tagTypeTo },

      tagTypeTo = OptionValue[SMRMonToMetadataRecommender, "TagTypeTo"];
      If[ !StringQ[tagTypeTo],
        Echo["The value of the option \"TagTypeTo\" is expected to be a string.", "SMRMonToMetadataRecommender:"];
        Return[$SMRMonFailure]
      ];

      SMRMonToMetadataRecommender[tagTypeTo, opts][xs, context]
    ];

SMRMonToMetadataRecommender[ tagTypeTo_String, opts : OptionsPattern[] ][ xs_, context_ ] :=
    Block[{numberOfTopTags, knownTagTypes, tagTypesFrom, tagSelectionCriteria,
      aLongForms, smat, aIDToTag, aSMats },

      knownTagTypes = SMRMonBind[ SMRMonUnit[xs, context], SMRMonTakeTagTypes];
      If[ TrueQ[knownTagTypes === $SMRMonFailure], Return[$SMRMonFailure]];

      If[ !MemberQ[knownTagTypes, tagTypeTo],
        Echo["The first argument is not a known recommender tag type.", "SMRMonToMetadataRecommender:"];
        Return[$SMRMonFailure]
      ];

      numberOfTopTags = OptionValue[SMRMonToMetadataRecommender, "NumberOfTopTags"];
      If[ TrueQ[numberOfTopTags === Automatic], numberOfTopTags = 1];
      If[ !(IntegerQ[numberOfTopTags] && numberOfTopTags > 0),
        Echo[
          "The value of the option \"NumberOfTopTags\" is expected to be a positive integer.",
          "SMRMonToMetadataRecommender:"
        ];
        Return[$SMRMonFailure]
      ];

      If[ numberOfTopTags > 1,
        Echo[
          "The value of the option \"NumberOfTopTags\" is taken to be 1: larger number of tags is not implemented (yet.).",
          "SMRMonToMetadataRecommender:"
        ];
        numberOfTopTags = 1
      ];

      tagTypesFrom = OptionValue[SMRMonToMetadataRecommender, "TagTypesFrom"];
      If[ TrueQ[tagTypesFrom === Automatic], tagTypesFrom = knownTagTypes];
      If[ TrueQ[tagTypesFrom === All], tagTypesFrom = knownTagTypes];
      tagTypesFrom = Flatten[{tagTypesFrom}];
      tagTypesFrom = Complement[tagTypesFrom, {tagTypeTo}];
      If[ !( VectorQ[tagTypesFrom, StringQ] && Length[tagTypesFrom] > 0 && Length[Complement[ tagTypesFrom, knownTagTypes] ] == 0 ),
        Echo[
          "The value of the option \"TagTypesFrom\" is expected to be All or a list of known recommender tag types.",
          "SMRMonToMetadataRecommender:"
        ];
        Return[$SMRMonFailure]
      ];

      tagSelectionCriteria = OptionValue[SMRMonToMetadataRecommender, "TagSelectionCriteria"];
      If[ TrueQ[tagSelectionCriteria === Automatic], tagSelectionCriteria = All];

      If[ IntegerQ[tagSelectionCriteria] && tagSelectionCriteria <= 0,
        Echo[
          "The value of the option \"TagSelectionCriteria\" is an integer then it is expected to be greater than zero.",
          "SMRMonToMetadataRecommender:"
        ];
        Return[$SMRMonFailure]
      ];

      (* Long forms *)
      aLongForms = Map[ SSparseMatrixAssociation, KeyTake[ context["matrices"], Append[ tagTypesFrom, tagTypeTo] ] ];

      (* Make mapping rules *)
      smat = context["matrices"][tagTypeTo];

      aIDToTag = TakeLargest[1] /@ RowAssociations[smat];
      aIDToTag = AssociationThread[ Keys[aIDToTag], First@*Keys /@ Values[aIDToTag] ];

      (* Take only specified tag types long forms association *)
      aLongForms = KeyTake[aLongForms, tagTypesFrom];

      If[ Length[aLongForms] == 0,
        Echo["Empty tag type long forms list was obtained.", "SMRMonToMetadataRecommender:"];
        Return[$SMRMonFailure]
      ];

      (* Map items to tagTypeTo values *)
      aLongForms = Map[ Map[ (#[[1]] /. aIDToTag) -> #[[2]]&, Normal@KeySelect[ #, KeyExistsQ[aIDToTag, #[[1]]]& ]]&, aLongForms];

      (* Create contingency matrices from the transformed long form *)
      aSMats = Map[ToSSparseMatrix @ CrossTabulate[Flatten /@ List @@@ #]&, aLongForms];

      (* Select tags *)
      (* This is very ineffective because of the transformation to row-associations. *)
      If[ !TrueQ[tagSelectionCriteria === All],
        aLongForms = Map[ RowAssociations, aSMats];

        Which[
          IntegerQ[tagSelectionCriteria] && tagSelectionCriteria > 0,
          aLongForms = Map[TakeLargest[#, tagSelectionCriteria]& /@ #&, aLongForms],

          True,
          aLongForms = Map[Select[#, tagSelectionCriteria]& /@ #&, aLongForms];
          aLongForms = Map[ Select[#, Length[#] > 0&]&, aLongForms]
        ];

        aSMats = ToSSparseMatrix /@ aLongForms;
      ];

      (* Create recommender *)
      SMRMonCreate[aSMats][xs, context]
    ];

SMRMonToMetadataRecommender[___][__] :=
    Block[{},
      Echo[
        "The expected signature is SMRMonToMetadataRecommender[ tagTypeTo_String, opts___ ] or SMRMonToMetadataRecommender[ opts__ ].",
        "SMRMonToMetadataRecommender:"];
      $SMRMonFailure
    ];


(*=========================================================*)
(* Import recommender                                      *)
(*=========================================================*)

Clear[SMRMonImportRecommender];

SyntaxInformation[SMRMonImportRecommender] = { "ArgumentsPattern" -> {_, OptionsPattern[] } };

SMRMonImportRecommender::uniq = "The `1` are expected to be unique.";
SMRMonImportRecommender::nfix = "The values of the \"Prefix\" and \"Infix\" are expected to be strings or Automatic.";

Options[SMRMonImportRecommender] = {"Prefix" -> "", "Infix" -> ""};

SMRMonImportRecommender[dirName_String, opts : OptionsPattern[]] :=
    Block[{prefix, infix, smat, dsTagTypeRanges, dsRowNames, rowNames, dsColumnNames, columnNames, smat2, smats},

      (* Obtain prefix and infix  *)
      prefix = OptionValue[SMRMonImportRecommender, "Prefix"];
      If[ TrueQ[prefix === Automatic], prefix = ""];

      infix = OptionValue[SMRMonImportRecommender, "Infix"];
      If[ TrueQ[infix === Automatic], infix = ""];

      If[ !StringQ[infix] || !StringQ[prefix],
        Message[SMRMonImportRecommender::nfix];
        Return[$Failed]
      ];

      If[ StringLength[prefix] > 0 && !StringMatchQ[prefix, __ ~~ "-"],
        prefix = prefix <> "-";
      ];

      If[ StringLength[infix] > 0 && !StringMatchQ[infix, "-" ~~ __],
        infix = "-" <> infix;
      ];

      (* Import recommender matrix *)
      smat = Import[FileNameJoin[{dirName, prefix <> "SMR-M01" <> infix <> ".mm"}]];

      dsTagTypeRanges = ImportCSVToDataset[FileNameJoin[{dirName, prefix <> "SMR-TagTypeRanges" <> infix <> ".csv"}], "RowNames" -> True];

      dsRowNames = ImportCSVToDataset[FileNameJoin[{dirName, prefix <> "SMR-M01-rownames" <> infix <> ".csv"}], "RowNames" -> True, "Numeric" -> False];
      rowNames = ToString /@ Normal[dsRowNames[Values, "RowName"]];

      If[Length[rowNames] != Length[Union[rowNames]],
        Message[SMRMonImportRecommender::uniq, "row names"];
        Return[$Failed]
      ];

      dsColumnNames = ImportCSVToDataset[FileNameJoin[{dirName, prefix <> "SMR-M01-colnames" <> infix <> ".csv"}], "RowNames" -> True, "Numeric" -> False];
      columnNames = ToString /@ Normal[dsColumnNames[Values, "ColumnName"]];

      If[Length[columnNames] != Length[Union[columnNames]],
        Message[SMRMonImportRecommender::uniq, "column names"];
        Return[$Failed]
      ];

      (* Create SMRMon object *)
      smat2 = ToSSparseMatrix[smat, "RowNames" -> rowNames, "ColumnNames" -> columnNames];
      smats = Map[smat2[[All, #Begin ;; #End]] &, Normal[dsTagTypeRanges]];

      SMRMonBind[ SMRMonUnit[], SMRMonCreate[smats] ]
    ];

SMRMonImportRecommender[___] :=
    Block[{},
      Echo[
        "The expected signature is SMRMonImportRecommender[ dirName_String, opts___ ].",
        "SMRMonImportRecommender:"];
      $SMRMonFailure
    ];


(*=========================================================*)
(* Summary Box                                             *)
(*=========================================================*)

SMRMon /: MakeBoxes[obj_SMRMon, form : StandardForm] :=
    Block[{ctx = SMRMonBind[obj, SMRMonTakeContext]},
      BoxForm`ArrangeSummaryBox[
        SMRMon, obj,
        None, (*the next argument is the always visible properties*)
        If[KeyExistsQ[ctx, "M"],
          {
            BoxForm`SummaryItem@{"Dimensions: ", Dimensions[SMRMonBind[obj, SMRMonTakeMatrix]]},
            BoxForm`SummaryItem@{"Fill in: ", N[Length[SparseArray[ctx["M"]]["NonzeroValues"]] / Apply[Times, Dimensions[ctx["M"]]]]},
            BoxForm`SummaryItem@{"Number of tag types: ", Length @ ctx["matrices"]}
          },
          (*ELSE*)
          {BoxForm`SummaryItem@{"No matrix"}}
        ],
        {
          If[KeyExistsQ[ctx, "matrices"],
            BoxForm`SummaryItem@{"Tag types: ", Short @ Keys @ ctx["matrices"]},
            Nothing
          ],
          If[KeyExistsQ[ctx, "M"],
            BoxForm`SummaryItem@{"Items: ", Short @ RowNames @ ctx["M"]},
            Nothing
          ],
          If[KeyExistsQ[ctx, "M"],
            BoxForm`SummaryItem@{"Tags: ", Short @ ColumnNames @ ctx["M"]},
            Nothing
          ]
        },
        form]
    ];

End[]; (* `Private` *)

EndPackage[]