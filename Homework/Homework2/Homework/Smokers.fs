module Smokers

open PetriNet

/// The places of the smokers model.
type Place =
    // Places representing the state of the table.
    | Tobacco // Indicates that there's tobacco on the table.
    | EmptyTobacco // Indicates that there's no tobacco on the table.
    | Paper // Indicates that there's paper on the table.
    | EmptyPaper // Indicates that there's no paper on the table.
    | Match // Indicates that there's a match on the table.
    | EmptyMatches // Indicates that there are no matches on the table.
    | FreeSlots // Indicates the number of free slots on the table for the referee to put ingredients in.

    // Places representing the state of the smoker who has tobacco.
    | SmokerWithTobaccoWaitsForBoth
    | SmokerWithTobaccoWaitsForPaper
    | SmokerWithTobaccoWaitsForMatch

    // Places representing the state of the smoker who has paper.
    | SmokerWithPaperWaitsForBoth
    | SmokerWithPaperWaitsForTobacco
    | SmokerWithPaperWaitsForMatch

    // Places representing the state of the smoker who has matches.
    | SmokerWithMatchesWaitsForBoth
    | SmokerWithMatchesWaitsForTobacco
    | SmokerWithMatchesWaitsForPaper

/// The transitions of the smokers model.
type Transition =
    // Transitions representing the behaviour of the referee.
    | PutTobacco
    | PutPaper
    | PutMatch

    // Transitions representing the behaviour of the smoker who has tobacco.
    | SmokerWithTobaccoTakesPaperFirst
    | SmokerWithTobaccoTakesPaperSecond
    | SmokerWithTobaccoTakesMatchFirst
    | SmokerWithTobaccoTakesMatchSecond

    // Transitions representing the behaviour of the smoker who has paper.
    | SmokerWithPaperTakesTobaccoFirst
    | SmokerWithPaperTakesTobaccoSecond
    | SmokerWithPaperTakesMatchFirst
    | SmokerWithPaperTakesMatchSecond

    // Transitions representing the behaviour of the smoker who has matches.
    | SmokerWithMatchesTakesTobaccoFirst
    | SmokerWithMatchesTakesTobaccoSecond
    | SmokerWithMatchesTakesPaperFirst
    | SmokerWithMatchesTakesPaperSecond

/// The pre-condition function of the smokers model.
let pre =
    Arcs.make [ // Pre-conditions for the referee:
                ((FreeSlots, PutTobacco), 1)
                ((EmptyTobacco, PutTobacco), 1)

                ((FreeSlots, PutPaper), 1)
                ((EmptyPaper, PutPaper), 1)

                ((FreeSlots, PutMatch), 1)
                ((EmptyMatches, PutMatch), 1)

                // Pre-conditions for the smoker with tobacco:
                ((SmokerWithTobaccoWaitsForBoth, SmokerWithTobaccoTakesPaperFirst), 1)
                ((Paper, SmokerWithTobaccoTakesPaperFirst), 1)

                ((SmokerWithTobaccoWaitsForBoth, SmokerWithTobaccoTakesMatchFirst), 1)
                ((Match, SmokerWithTobaccoTakesMatchFirst), 1)

                ((SmokerWithTobaccoWaitsForPaper, SmokerWithTobaccoTakesPaperSecond), 1)
                ((Paper, SmokerWithTobaccoTakesPaperSecond), 1)

                ((SmokerWithTobaccoWaitsForMatch, SmokerWithTobaccoTakesMatchSecond), 1)
                ((Match, SmokerWithTobaccoTakesMatchSecond), 1)

                // Pre-conditions for the smoker with paper:
                ((SmokerWithPaperWaitsForBoth, SmokerWithPaperTakesTobaccoFirst), 1)
                ((Tobacco, SmokerWithPaperTakesTobaccoFirst), 1)

                ((SmokerWithPaperWaitsForBoth, SmokerWithPaperTakesMatchFirst), 1)
                ((Match, SmokerWithPaperTakesMatchFirst), 1)

                ((SmokerWithPaperWaitsForTobacco, SmokerWithPaperTakesTobaccoSecond), 1)
                ((Tobacco, SmokerWithPaperTakesTobaccoSecond), 1)

                ((SmokerWithPaperWaitsForMatch, SmokerWithPaperTakesMatchSecond), 1)
                ((Match, SmokerWithPaperTakesMatchSecond), 1)

                // Pre-conditions for the smoker with matches:
                ((SmokerWithMatchesWaitsForBoth, SmokerWithMatchesTakesTobaccoFirst), 1)
                ((Tobacco, SmokerWithMatchesTakesTobaccoFirst), 1)

                ((SmokerWithMatchesWaitsForBoth, SmokerWithMatchesTakesPaperFirst), 1)
                ((Paper, SmokerWithMatchesTakesPaperFirst), 1)

                ((SmokerWithMatchesWaitsForTobacco, SmokerWithMatchesTakesTobaccoSecond), 1)
                ((Tobacco, SmokerWithMatchesTakesTobaccoSecond), 1)

                ((SmokerWithMatchesWaitsForPaper, SmokerWithMatchesTakesPaperSecond), 1)
                ((Paper, SmokerWithMatchesTakesPaperSecond), 1) ]

let post =
    Arcs.make [ // Post-conditions for the referee:
                ((Tobacco, PutTobacco), 1)

                ((Paper, PutPaper), 1)

                ((Match, PutMatch), 1)

                // Post-conditions for the smoker with tobacco:
                ((FreeSlots, SmokerWithTobaccoTakesPaperFirst), 1)
                ((EmptyPaper, SmokerWithTobaccoTakesPaperFirst), 1)
                ((SmokerWithTobaccoWaitsForMatch, SmokerWithTobaccoTakesPaperFirst), 1)

                ((FreeSlots, SmokerWithTobaccoTakesMatchFirst), 1)
                ((EmptyMatches, SmokerWithTobaccoTakesMatchFirst), 1)
                ((SmokerWithTobaccoWaitsForPaper, SmokerWithTobaccoTakesMatchFirst), 1)

                ((FreeSlots, SmokerWithTobaccoTakesPaperSecond), 1)
                ((EmptyPaper, SmokerWithTobaccoTakesPaperSecond), 1)
                ((SmokerWithTobaccoWaitsForBoth, SmokerWithTobaccoTakesPaperSecond), 1)

                ((FreeSlots, SmokerWithTobaccoTakesMatchSecond), 1)
                ((EmptyMatches, SmokerWithTobaccoTakesMatchSecond), 1)
                ((SmokerWithTobaccoWaitsForBoth, SmokerWithTobaccoTakesMatchSecond), 1)

                // Post-conditions for the smoker with paper:
                ((FreeSlots, SmokerWithPaperTakesTobaccoFirst), 1)
                ((EmptyTobacco, SmokerWithPaperTakesTobaccoFirst), 1)
                ((SmokerWithPaperWaitsForMatch, SmokerWithPaperTakesTobaccoFirst), 1)

                ((FreeSlots, SmokerWithPaperTakesMatchFirst), 1)
                ((EmptyMatches, SmokerWithPaperTakesMatchFirst), 1)
                ((SmokerWithPaperWaitsForTobacco, SmokerWithPaperTakesMatchFirst), 1)

                ((FreeSlots, SmokerWithPaperTakesTobaccoSecond), 1)
                ((EmptyTobacco, SmokerWithPaperTakesTobaccoSecond), 1)
                ((SmokerWithPaperWaitsForBoth, SmokerWithPaperTakesTobaccoSecond), 1)

                ((FreeSlots, SmokerWithPaperTakesMatchSecond), 1)
                ((EmptyMatches, SmokerWithPaperTakesMatchSecond), 1)
                ((SmokerWithPaperWaitsForBoth, SmokerWithPaperTakesMatchSecond), 1)

                // Post-conditions for the smoker with matches:
                ((FreeSlots, SmokerWithMatchesTakesTobaccoFirst), 1)
                ((EmptyTobacco, SmokerWithMatchesTakesTobaccoFirst), 1)
                ((SmokerWithMatchesWaitsForPaper, SmokerWithMatchesTakesTobaccoFirst), 1)

                ((FreeSlots, SmokerWithMatchesTakesPaperFirst), 1)
                ((EmptyPaper, SmokerWithMatchesTakesPaperFirst), 1)
                ((SmokerWithMatchesWaitsForTobacco, SmokerWithMatchesTakesPaperFirst), 1)

                ((FreeSlots, SmokerWithMatchesTakesTobaccoSecond), 1)
                ((EmptyTobacco, SmokerWithMatchesTakesTobaccoSecond), 1)
                ((SmokerWithMatchesWaitsForBoth, SmokerWithMatchesTakesTobaccoSecond), 1)

                ((FreeSlots, SmokerWithMatchesTakesPaperSecond), 1)
                ((EmptyPaper, SmokerWithMatchesTakesPaperSecond), 1)
                ((SmokerWithMatchesWaitsForBoth, SmokerWithMatchesTakesPaperSecond), 1) ]

/// The smokers model.
let model = Model.make pre post

/// The initial marking of the smokers model.
let initialMarking =
    Marking.make [ // Initial state of the table.
                   (EmptyTobacco, 1)
                   (EmptyPaper, 1)
                   (EmptyMatches, 1)
                   (FreeSlots, 2)

                   // Initial state of the smokers.
                   (SmokerWithTobaccoWaitsForBoth, 1)
                   (SmokerWithPaperWaitsForBoth, 1)
                   (SmokerWithMatchesWaitsForBoth, 1) ]

/// An initial marking from which the smokers model encounters deadlocks.
let markingWithDeadlocks =
    Marking.make [ (FreeSlots, 0) ]
