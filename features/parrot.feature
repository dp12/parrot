Feature: Rotate words
  In order to switch between common variations of a word
  As an Emacs user and parrot lover
  I want to rotate text

  Background:
    Given I switch to buffer "*parrot*"
    And the buffer is empty

  Scenario: Lowercase rotation
    Given I set parrot-rotate-dict to ((:rot ("snek" "snake")))
    And I insert:
    """
    this_is_snek_charmer_stawp
    """
    And I place the cursor between "is_" and "snek"
    And I call "parrot-rotate-next-word-at-point"
    Then I should see:
    """
    this_is_snake_charmer_stawp
    """
    Then the cursor should be between "is_" and "snake"

  Scenario: Next and previous rotations with wrapping
    Given I set parrot-rotate-dict to ((:rot ("shiba-inu" "such_animate" "wow")))
    And I insert:
    """
    shiba-inu
    """
    And I go to word "shiba-inu"
    And I call "parrot-rotate-next-word-at-point"
    Then I should see:
    """
    such_animate
    """
    And I call "parrot-rotate-next-word-at-point"
    Then I should see:
    """
    wow
    """
    And I call "parrot-rotate-next-word-at-point"
    Then I should see:
    """
    shiba-inu
    """
    And I call "parrot-rotate-prev-word-at-point"
    Then I should see:
    """
    wow
    """
    And I call "parrot-rotate-prev-word-at-point"
    Then I should see:
    """
    such_animate
    """
    And I call "parrot-rotate-prev-word-at-point"
    Then I should see:
    """
    shiba-inu
    """

  Scenario: Jump to rotation on the right
    Given I set parrot-rotate-dict to ((:rot ("hunt" "stunt")))
    And I insert:
    """
    tehwildhunt
    """
    And I place the cursor between "teh" and "wild"
    And I call "parrot-rotate-next-word-at-point"
    Then I should see:
    """
    tehwildstunt
    """
    Then the cursor should be between "wild" and "stunt"

  Scenario: Jump to rotation on the left
    Given I set parrot-rotate-dict to ((:rot ("teh" "the")))
    And I insert:
    """
    tehwildhunt
    """
    And I place the cursor between "wild" and "hunt"
    And I call "parrot-rotate-next-word-at-point"
    Then I should see:
    """
    thewildhunt
    """
    Then the cursor should be between "th" and "ewild"

  Scenario: When both left/right matches are found, jump to closest match 
    Given I set parrot-rotate-dict to ((:rot ("true" "false")) (:rot ("this" "that")))
    And I insert:
    """
    truehumebugthis
    """
    And I place the cursor between "hum" and "ebug"
    And I call "parrot-rotate-next-word-at-point"
    Then I should see:
    """
    truehumebugthat
    """
    Then the cursor should be between "bug" and "that"
    And I place the cursor between "hume" and "bug"
    And I call "parrot-rotate-next-word-at-point"
    Then I should see:
    """
    truehumebugthis
    """
    And I place the cursor between "hu" and "mebug"
    And I call "parrot-rotate-next-word-at-point"
    Then I should see:
    """
    falsehumebugthis
    """
    Then the cursor should be between "fals" and "ehume"

  Scenario: When parrot-rotate-hunt-for-words is nil, don't jump to other matches
    Given I set parrot-rotate-dict to ((:rot ("true" "false")) (:rot ("this" "that")))
    And I set parrot-rotate-hunt-for-words to nil
    And I insert:
    """
    truehumebugthis
    """
    And I place the cursor between "hum" and "ebug"
    Then parrot-rotate-next-word-at-point should throw an error
    Then I should see:
    """
    truehumebugthis
    """

  Scenario: When cursor is on blank space, don't rotate.
    Given I set parrot-rotate-dict to ((:rot ("lost" "space")))
    And I insert:
    """
    lost in space
    """
    And I place the cursor between "t " and "in"
    Then parrot-rotate-next-word-at-point should throw an error
    Then I should see:
    """
    lost in space
    """
    Then the cursor should be between "t " and "in"
    And I place the cursor between "in" and " space"
    Then parrot-rotate-next-word-at-point should throw an error
    Then I should see:
    """
    lost in space
    """
    Then the cursor should be between "in" and " space"

  Scenario: When parrot-rotate-jump-to-word-after-hunt is nil, rotate match, but don't jump to match
    Given I set parrot-rotate-dict to ((:rot ("true" "false")) (:rot ("this" "that")))
    And I set parrot-rotate-jump-to-word-after-hunt to nil
    And I insert:
    """
    truehumebugthis
    """
    And I place the cursor between "hum" and "ebug"
    And I call "parrot-rotate-next-word-at-point"
    Then I should see:
    """
    truehumebugthat
    """
    Then the cursor should be between "hum" and "ebug"
    And I place the cursor between "hume" and "bug"
    And I call "parrot-rotate-next-word-at-point"
    Then I should see:
    """
    truehumebugthis
    """
    Then the cursor should be between "hume" and "bug"
    And I place the cursor between "hu" and "mebug"
    And I call "parrot-rotate-next-word-at-point"
    Then I should see:
    """
    falsehumebugthis
    """
    Then the cursor should be between "falseh" and "umebug"

  Scenario: Test invalid dictionary with no rotations
    Given I set parrot-rotate-dict to ((:rot ("hakuna" "matata") :lower nil))
    And I insert:
    """
    hakuna
    """
    And I place the cursor between "hak" and "una"
    Then parrot-rotate-next-word-at-point should throw an error
    Then the cursor should be between "hak" and "una"

  Scenario: Test invalid dictionary with one rotation
    Given I set parrot-rotate-dict to ((:rot ("hakuna")))
    And I insert:
    """
    hakuna
    """
    And I place the cursor between "hak" and "una"
    Then parrot-rotate-next-word-at-point should throw an error
    Then the cursor should be between "hak" and "una"

  Scenario: Test invalid dictionary with one rotation
    Given I set parrot-rotate-dict to ((:rot ("needle" "haystack")) (:rot ("needle" "noodle")))
    And I insert:
    """
    needle_in_a_haystack
    """
    And I place the cursor between "nee" and "dle"
    Then parrot-rotate-next-word-at-point should throw an error
    Then the cursor should be between "nee" and "dle"

  Scenario: Rotation with capitalized option set
    Given I set parrot-rotate-dict to ((:rot ("case" "casey") :caps t))
    And I insert:
    """
    camelCase
    """
    And I call "parrot-rotate-next-word-at-point"
    Then I should see:
    """
    camelCasey
    """
    And I call "parrot-rotate-next-word-at-point"
    Then I should see:
    """
    camelCase
    """

  Scenario: Rotation with upcase option set
    Given I set parrot-rotate-dict to ((:rot ("peeple" "sheeple") :upcase t))
    And I insert:
    """
    PEEPLE
    """
    And I call "parrot-rotate-next-word-at-point"
    Then I should see:
    """
    SHEEPLE
    """
    And I call "parrot-rotate-next-word-at-point"
    Then I should see:
    """
    PEEPLE
    """

  Scenario: Rotation with all options set
    Given I set parrot-rotate-dict to ((:rot ("tick" "tock") :caps t :upcase t))
    And I insert:
    """
    tickTickTICK
    """
    And I place the cursor after "ti"
    And I call "parrot-rotate-next-word-at-point"
    Then I should see:
    """
    tockTickTICK
    """
    And I place the cursor after "Ti"
    And I call "parrot-rotate-next-word-at-point"
    Then I should see:
    """
    tockTockTICK
    """
    And I place the cursor after "TI"
    And I call "parrot-rotate-next-word-at-point"
    Then I should see:
    """
    tockTockTOCK
    """

  Scenario: Rotation with lowercase excluded
    Given I set parrot-rotate-dict to ((:rot ("yolo" "nolo") :upcase t :lower nil))
    And I insert:
    """
    yoloYOLO
    """
    And I place the cursor after "yo"
    And I call "parrot-rotate-next-word-at-point"
    Then I should see:
    """
    yoloNOLO
    """

  Scenario: Rotation with increasing case
    Given I set parrot-rotate-dict to ((:rot ("wow" "Wow" "WOW")))
    And I insert:
    """
    wow
    """
    And I call "parrot-rotate-next-word-at-point"
    Then I should see:
    """
    Wow
    """
    And I call "parrot-rotate-next-word-at-point"
    Then I should see:
    """
    WOW
    """

  Scenario: Rotation with underscores, hyphens, and apostrophes
    Given I set parrot-rotate-dict to ((:rot ("is_snek" "isn't-snake")))
    And I insert:
    """
    this_is_snek_stawp
    """
    And I call "parrot-rotate-next-word-at-point"
    Then I should see:
    """
    this_isn't-snake_stawp
    """
    And I call "parrot-rotate-next-word-at-point"
    Then I should see:
    """
    this_is_snek_stawp
    """

  Scenario: Rotation with special characters
    Given I set parrot-rotate-dict to ((:rot ("!@$#%^" "&*-_=+()")))
    And I insert:
    """
    !@$#%^
    """
    And I call "parrot-rotate-next-word-at-point"
    Then I should see:
    """
    &*-_=+()
    """
    And I call "parrot-rotate-next-word-at-point"
    Then I should see:
    """
    !@$#%^
    """

  Scenario: Rotation with numbers
    Given I set parrot-rotate-dict to ((:rot ("2" "4" "8" "16")))
    And I insert:
    """
    2
    """
    And I call "parrot-rotate-prev-word-at-point"
    Then I should see:
    """
    16
    """
    And I call "parrot-rotate-prev-word-at-point"
    Then I should see:
    """
    8
    """
    And I call "parrot-rotate-prev-word-at-point"
    Then I should see:
    """
    4
    """

  Scenario: Cursor adjustment with longer to shorter rotation
    Given I set parrot-rotate-dict to ((:rot ("false" "true")))
    And I insert:
    """
    This is a false statement
    """
    And I place the cursor after "fals"
    And I call "parrot-rotate-next-word-at-point"
    Then I should see:
    """
    This is a true statement
    """
    Then the cursor should be after "tru"
    And I clear the buffer
    And I insert:
    """
    This-is-a-false-statement
    """
    And I place the cursor after "state"
    And I call "parrot-rotate-next-word-at-point"
    Then I should see:
    """
    This-is-a-true-statement
    """
    Then the cursor should be after "tru"
