
(**************************************************************)
(* Package definition                                         *)
(**************************************************************)

BeginPackage["AntonAntonov`MonadicSparseMatrixRecommender`"];

$SMRMonFailure::usage = "Failure symbol of SMRMon.";

SMRMonAnnexSubMatrices::usage = "Annexes matrices to the recommender.";

SMRMonAnnexSubMatrix::usage = "Annexes matrices to the recommender. (Synonym of SMRMonAnnexSubMatrices.)";

SMRMonApplyGlobalWeightFunction::usage = "Applies a specified global weight function to the entries of the contingency matrix.";

SMRMonApplyLocalWeightFunction::usage = "Applies a specified local weight function to the entries of the contingency matrix.";

SMRMonApplyNormalizationFunction::usage = "Applies a specified normalization function to the entries of the contingency matrix.";

SMRMonApplyTermWeightFunctions::usage = "Applies term weight functions to entries of the recommender matrix.";

SMRMonBatchRecommend::usage = "Recommends items using a SSparseMatrix object or a items specification.";

SMRMonClassify::usage = "Uses IIR as a classifier for specified label tag-type over a vector or a matrix.";

SMRMonClassifyOriginal::usage = "Uses IIR as a classifier for specified label tag-type over a vector or a matrix.";

SMRMonComputeTopK::usage = "SMRMonComputeTopK[ testData_Association, ks:{_?IntegerQ..}, opts] computes the Top-K for specified data and K's.";

SMRMonCreate::usage = "Creates the recommender structures from a transactions Dataset or an association of sparse matrices.";

SMRMonCreateFromLongForm::usage = "Creates the recommender structures from a long form Dataset -- each row is expected to have values corresponding to item ID, tag type, tag, weight.";

SMRMonCreateFromWideForm::usage = "Creates the recommender structures from a transactions Dataset.";

SMRMonEchoDataSummary::usage = "Echoes summary of the dataset.";

SMRMonFilterByProfile::usage = "SMRMonFilterByProfile[ prof : ( { _String ..} | Association[ (_Integer -> _?NumberQ) .. ] | Association[ (_String -> _?NumberQ) .. ] ) ] finds the items that have the tags of the given profile. The scores are corresponding row sums.";

SMRMonFilterMatrix::usage = "SMRMonFilterMatrix[ prof : ( { _String ..} | Association[ (_Integer -> _?NumberQ) .. ] | Association[ (_String -> _?NumberQ) .. ] ) ] applies a profile filter to the rows of the recommendation matrix.";

SMRMonFromProfileVector::usage = "Makes a profile association from a profile vector argument.";

SMRMonGetMatrixProperty::usage = "Gets a recommender matrix property.";

SMRMonGetProperty::usage = "Gets a recommender property.";

SMRMonGetTagTypeRanges::usage = "Gets the ranges of the tag types in the recommendation matrix.";

SMRMonGetTopRecommendations::usage = "Recommends items based on a history or profile specification.";

SMRMonImportRecommender::usage = "SMRMonImportRecommender[ dirName_String] imports a recommender using data files from the specified directory having the specified prefix and infix through the options \"Prefix\" and \"Infix\". This is a non-monadic function; can be used in place of SMRMonUnit[].";

SMRMonJoin::usage = "Joins the recommender with another recommender. (By column-binding the corresponding tag-type sub-matrices.)";

SMRMonJoinAcross::usage = "Joins a recommendations association with a given Dataset object.";

SMRMonProfile::usage = "Computes profile based on history.";

SMRMonProveByHistory::usage = "Computes proofs for recommendations using consumption history.";

SMRMonProveByHistory::usage = "History proofs for a recommended item and scored history items.";

SMRMonProveByMetadata::usage = "Metadata proofs for a recommended item and a profile. (Tags from item's profile that are found in the given profile.)";

SMRMonProveByProfile::usage = "Computes proofs for recommendations using consumption profile.";

SMRMonRecommend::usage = "Recommends items based on history.";

SMRMonRecommendByCorrelation::usage = "Recommends items based on a correlation matrix. (The context value for the key \"timeSeriesMatrix\" should have the same dimensions and row names as the recommendation matrix.)";

SMRMonRecommendByHistory::usage = "Recommends items based on history.";

SMRMonRecommendByProfile::usage = "Recommends items based on profile.";

SMRMonRemoveTagTypes::usage = "SMRMonRemoveTagTypes[ tagTypes : {_String..}] removes specified tag types.";

SMRMonRetrieveByQueryElements::usage = "SMRMonRetrieveByQueryElements[should, must, mustNot] retrieves items according the retrieval query elements.";

SMRMonRowBind::usage = "Row-binds the recommender with another recommender. (By row-binding the corresponding tag-type sub-matrices.)";

SMRMonScoredItemsQ::usage = "True if the argument is an association with item names or item indices as keys and numbers as values.";

SMRMonScoredTagsQ::usage = "True if the argument is an association with tags or tag indices as keys and numbers as values.";

SMRMonSetClassificationParameters::usage = "Sets the parameters to be used by SMRMonClassifyOriginal.";

SMRMonSetTagTypeWeights::usage = "Sets weights (significance factors) to the IIR tag types.";

SMRMonSetTagWeights::usage = "Sets weights (significance factors) to the IIR tags.";

SMRMonSetTimeSeriesMatrix::usage = "Sets a time series matrix to be used with SMRMonRecommendByCorrelation.";

SMRMonTakeItemNames::usage = "Gives the item names. (Row names of the recommender matrix.)";

SMRMonTakeMatrices::usage = "Gives an association with the tag (sparse) matrices.";

SMRMonTakeMatrix::usage = "Gives the recommendation matrix (SSparseMatrix).";

SMRMonTakeMatrixDataset::usage = "Takes the Dataset object corresponding to the recommendation matrix.";

SMRMonTakeTagTypeWeights::usage = "Takes the tag-type weights.";

SMRMonTakeTagTypes::usage = "Takes the tag-types.";

SMRMonTakeTags::usage = "Gives the tags. (Column names of the recommender matrix.)";

SMRMonToItemsDataset::usage = "Converts a recommendations association into a Dataset object.";

SMRMonToMetadataRecommender::usage = "SMRMonToMetadataRecommender[ tagTypeTo_String, opts] converts the recommender into a recommender for tagTypeTo tags.";

SMRMonToProfileVector::usage = "SMRMonToProfileVector[ prof : ( { _String ..} | Association[ (_Integer -> _?NumberQ) .. ] | Association[ (_String -> _?NumberQ) .. ] ) ] makes a profile vector from an argument that is a list of tags or an association of scored indices or scored tags.";

SMRMonUnit::usage = "Monad's unit constructor.";

(* Paclets load *)

PacletInstall["AntonAntonov/DataReshapers", AllowVersionUpdate -> False];
PacletInstall["AntonAntonov/MonadicLatentSemanticAnalysis", AllowVersionUpdate -> False];
PacletInstall["AntonAntonov/MonadMakers", AllowVersionUpdate -> False];
PacletInstall["AntonAntonov/OutlierIdentifiers", AllowVersionUpdate -> False];
PacletInstall["AntonAntonov/SSparseMatrix", AllowVersionUpdate -> False];

Begin["`Private`"];

Needs["AntonAntonov`MonadicSparseMatrixRecommender`SMRMon`"];


End[]; (*`Private`*)

EndPackage[]