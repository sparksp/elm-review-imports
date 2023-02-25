module ReviewPipelineStyles.Custom exposing (noSinglePiplinesWithSimpleInputs)

import ReviewPipelineStyles
    exposing
        ( PipelineRule
        , andCallThem
        , andTryToFixThemBy
        , forbid
        , rightPizzaPipelines
        , that
        )
import ReviewPipelineStyles.Fixes
    exposing
        ( eliminatingInputStep
        )
import ReviewPipelineStyles.Predicates
    exposing
        ( and
        , haveASimpleInputStep
        , haveFewerStepsThan
        )


noSinglePiplinesWithSimpleInputs : List (PipelineRule ())
noSinglePiplinesWithSimpleInputs =
    [ forbid rightPizzaPipelines
        |> that
            (haveASimpleInputStep
                |> and (haveFewerStepsThan 2)
            )
        |> andTryToFixThemBy eliminatingInputStep
        |> andCallThem "|> pipeline with simple input"
    ]
