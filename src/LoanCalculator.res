%%raw(`import "./LoanCalculator.css"`)

type themeMode =
  | Oled
  | Light

@send external toLocaleString: (float, string) => string = "toLocaleString"

let formatCurrency = (value: float): string => {
  let rounded = Math.round(value *. 100.0) /. 100.0
  "₹" ++ toLocaleString(rounded, "en-IN")
}

let styleValue = (style: LoanMath.repaymentStyle): string =>
  LoanMath.repaymentStyleToString(style)

let profileAt = (profiles: array<LoanPersistence.profile>, index: int): LoanPersistence.profile =>
  switch Belt.Array.get(profiles, index) {
  | Some(profile) => profile
  | None => LoanPersistence.defaultProfile
  }

let profileOptionLabel = (profile: LoanPersistence.profile, index: int): string => {
  let name = profile.name == "" ? "Untitled Loan" : profile.name
  let purpose = profile.purpose == "" ? "" : " · " ++ profile.purpose
  if name == "" && purpose == "" {
    "Loan " ++ Belt.Int.toString(index + 1)
  } else {
    name ++ purpose
  }
}

let profilePaidMonths = (profile: LoanPersistence.profile): array<int> => switch profile.style {
| LoanMath.FlatRate => profile.flatPaidMonths
| LoanMath.Emi => profile.emiPaidMonths
| LoanMath.Bullet => profile.bulletPaidMonths
}

let profileWithPaidMonths = (
  profile: LoanPersistence.profile,
  paidMonths: array<int>,
): LoanPersistence.profile => switch profile.style {
| LoanMath.FlatRate => {...profile, flatPaidMonths: paidMonths}
| LoanMath.Emi => {...profile, emiPaidMonths: paidMonths}
| LoanMath.Bullet => {...profile, bulletPaidMonths: paidMonths}
}

let profileDisbursementInputs = (profile: LoanPersistence.profile): array<(string, string)> =>
  profile.disbursements->Belt.Array.map(disbursement =>
    (disbursement.amountInput, disbursement.monthInput)
  )

@react.component
let make = () => {
  let (profiles, setProfiles) = React.useState(() => [LoanPersistence.defaultProfile])
  let (activeProfileIndex, setActiveProfileIndex) = React.useState(() => 0)
  let (theme, setTheme) = React.useState(() => Oled)
  let (feedback, setFeedback) = React.useState(() => None)
  let (fundingPlanExpanded, setFundingPlanExpanded) = React.useState(() => false)
  let (profilesExpanded, setProfilesExpanded) = React.useState(() => false)

  let activeProfile = profileAt(profiles, activeProfileIndex)

  let updateActiveProfile = (update: LoanPersistence.profile => LoanPersistence.profile) =>
    setProfiles(prev =>
      prev->Belt.Array.mapWithIndex((index, profile) =>
        index == activeProfileIndex ? update(profile) : profile
      )
    )

  let fileInputRef = React.useRef(Nullable.null)

  let parsedInput = LoanMath.parseStagedLoanInput(
    ~plannedPrincipalInput=activeProfile.principalInput,
    ~annualRateInput=activeProfile.annualRateInput,
    ~tenureMonthsInput=activeProfile.tenureMonthsInput,
    ~disbursementInputs=profileDisbursementInputs(activeProfile),
  )
  let calculationResult = switch parsedInput {
  | Ok(input) => LoanMath.calculateStaged(~input, ~style=activeProfile.style)
  | Error(error) => Error(error)
  }
  let currentInput = switch parsedInput {
  | Ok(input) => input
  | Error(_) => {
      plannedPrincipal: LoanMath.defaultInput.principal,
      annualRate: LoanMath.defaultInput.annualRate,
      tenureMonths: LoanMath.defaultInput.tenureMonths,
      disbursements: [{amount: LoanMath.defaultInput.principal, month: 1}],
    }
  }

  let activePaidMonths = profilePaidMonths(activeProfile)
  let normalizedActivePaidMonths = LoanMath.normalizePaidMonths(
    ~months=currentInput.tenureMonths,
    ~paidMonths=activePaidMonths,
  )

  let setActivePaidMonths = (update: array<int> => array<int>) =>
    updateActiveProfile(profile => {
      let normalizedUpdate = LoanMath.normalizePaidMonths(
        ~months=currentInput.tenureMonths,
        ~paidMonths=update(profilePaidMonths(profile)),
      )
      profileWithPaidMonths(profile, normalizedUpdate)
    })

  let clearFeedback = () => setFeedback(_ => None)

  let togglePaidMonth = (month: int) => {
    setActivePaidMonths(paidMonths => {
      if Belt.Array.some(paidMonths, existing => existing == month) {
        Belt.Array.keep(paidMonths, existing => existing != month)
      } else {
        Belt.Array.concat(paidMonths, [month])
      }
    })
  }

  let updateActiveDisbursement = (
    index: int,
    update: LoanPersistence.disbursementInput => LoanPersistence.disbursementInput,
  ) => updateActiveProfile(profile => {
    let disbursements = profile.disbursements->Belt.Array.mapWithIndex((disbursementIndex, disbursement) =>
      disbursementIndex == index ? update(disbursement) : disbursement
    )
    {...profile, disbursements}
  })

  let handleAddDisbursement = () => {
    updateActiveProfile(profile => {
      let nextDisbursement: LoanPersistence.disbursementInput = {amountInput: "", monthInput: "1"}
      {...profile, disbursements: Belt.Array.concat(profile.disbursements, [nextDisbursement])}
    })
    setFeedback(_ => Some("Disbursement row added."))
  }

  let handleRemoveDisbursement = (index: int) => {
    if Belt.Array.length(activeProfile.disbursements) > 1 {
      updateActiveProfile(profile => {
        let disbursements = profile.disbursements->Belt.Array.keepWithIndex(
          (_, disbursementIndex) => disbursementIndex != index,
        )
        {...profile, disbursements}
      })
      setFeedback(_ => Some("Disbursement row removed."))
    }
  }

  let handleAddProfile = () => {
    let nextIndex = Belt.Array.length(profiles)
    let nextProfile = LoanPersistence.createProfile(
      ~name="New Loan " ++ Belt.Int.toString(nextIndex + 1),
      ~purpose="",
    )
    setProfiles(prev => Belt.Array.concat(prev, [nextProfile]))
    setActiveProfileIndex(_ => nextIndex)
    setFeedback(_ => Some("New loan profile created."))
  }

  let handleDeleteProfile = () => {
    let profileCount = Belt.Array.length(profiles)
    if profileCount > 1 {
      let nextProfiles = Belt.Array.keepWithIndex(
        profiles,
        (_, index) => index != activeProfileIndex,
      )
      let nextActiveIndex = activeProfileIndex >= profileCount - 1
        ? profileCount - 2
        : activeProfileIndex
      setProfiles(_ => nextProfiles)
      setActiveProfileIndex(_ => nextActiveIndex)
      setFeedback(_ => Some("Loan profile deleted."))
    }
  }

  let allProfilesValid = Belt.Array.every(profiles, profile => switch LoanMath.parseStagedLoanInput(
    ~plannedPrincipalInput=profile.principalInput,
    ~annualRateInput=profile.annualRateInput,
    ~tenureMonthsInput=profile.tenureMonthsInput,
    ~disbursementInputs=profileDisbursementInputs(profile),
  ) {
  | Ok(_) => true
  | Error(_) => false
  })

  let handleExport = () => switch calculationResult {
  | Ok(_) if allProfilesValid =>
    let json = LoanPersistence.encodeProfiles(~profiles, ~activeProfileIndex)
    LoanPersistence.downloadProfiles(json)
    setFeedback(_ => Some("Loan profiles exported."))
  | _ => setFeedback(_ => Some("Fix invalid loan values before exporting."))
  }

  let handleFileChange = (event: ReactEvent.Form.t) => {
    let target = ReactEvent.Form.target(event)
    let files = target["files"]
    if !Nullable.isNullable(files) && files["length"] > 0 {
      let file = files[0]
      LoanPersistence.clearFileInput(target)
      LoanPersistence.readFileAsText(
        file,
        content => switch LoanPersistence.decodeProfiles(content) {
        | Ok(saved) => {
            setProfiles(_ => saved.profiles)
            setActiveProfileIndex(_ => saved.activeProfileIndex)
            setFeedback(_ => Some("Loan profiles imported."))
          }
        | Error(error) => setFeedback(_ => Some(LoanPersistence.importErrorMessage(error)))
        },
        message => setFeedback(_ => Some(message)),
      )
    }
  }

  let containerClass = "loan-calculator-container " ++ (switch theme {
  | Oled => "theme-oled"
  | Light => "theme-light"
  })

  let content = switch calculationResult {
  | Error(error) =>
    <div className="validation-panel" role="alert">
      {React.string(LoanMath.validationMessage(error))}
    </div>
  | Ok(calculation) =>
    let paidCount = normalizedActivePaidMonths->Belt.Array.length
    let progressPct = Belt.Int.toFloat(paidCount) /. Belt.Int.toFloat(currentInput.tenureMonths) *. 100.0
    <>
      <div className="results-grid">
        <div className="stat-card stat-card-primary">
          <p className="stat-label"> {React.string("Total Interest")} </p>
          <p className="stat-value"> {React.string(formatCurrency(calculation.totalInterest))} </p>
        </div>
        <div className="stat-card stat-card-primary">
          <p className="stat-label"> {React.string("Total Repayment")} </p>
          <p className="stat-value"> {React.string(formatCurrency(calculation.totalRepaid))} </p>
        </div>
        <div className="stat-card stat-card-secondary">
          <p className="stat-label"> {React.string("Month 1 Payment")} </p>
          <p className="stat-value"> {React.string(formatCurrency(calculation.monthlyPayment))} </p>
        </div>
        <div className="stat-card stat-card-secondary">
          <p className="stat-label"> {React.string("Payment Progress")} </p>
          <p className="stat-value">
            {React.string(Belt.Int.toString(paidCount) ++ "/" ++ Belt.Int.toString(currentInput.tenureMonths) ++ " Paid")}
          </p>
          <progress
            className="progress-control"
            value={Belt.Float.toString(Belt.Int.toFloat(paidCount))}
            max={Belt.Float.toString(Belt.Int.toFloat(currentInput.tenureMonths))}
            ariaLabel="Payment progress"
          />
          <span className="visually-hidden"> {React.string(Belt.Float.toString(progressPct) ++ "% complete")} </span>
        </div>
      </div>

      <div className="schedule-section">
        <div className="schedule-header">
          <h2 className="section-title"> {React.string("Amortization & Payment Schedule")} </h2>
          <div className="schedule-actions">
            <button
              type_="button"
              className="btn-action"
              onClick={_ => setActivePaidMonths(_ => LoanMath.allPaidMonths(currentInput.tenureMonths))}>
              {React.string("Mark All Paid")}
            </button>
            <button
              type_="button"
              className="btn-action"
              onClick={_ => setActivePaidMonths(_ => [])}>
              {React.string("Reset Paid")}
            </button>
          </div>
        </div>

        <div className="table-wrapper">
          <table className="schedule">
            <caption className="visually-hidden"> {React.string("Loan payment schedule")} </caption>
            <thead>
              <tr>
                <th scope="col"> {React.string("Status")} </th>
                <th scope="col"> {React.string("Month")} </th>
                <th scope="col"> {React.string("Payment")} </th>
                <th scope="col"> {React.string("Interest")} </th>
                <th scope="col"> {React.string("Principal")} </th>
                <th scope="col"> {React.string("Balance")} </th>
              </tr>
            </thead>
            <tbody>
              {calculation.schedule
              ->Belt.Array.map(row => {
                let isPaid = Belt.Array.some(normalizedActivePaidMonths, month => month == row.month)
                <tr
                  key={Belt.Int.toString(row.month)}
                  className={isPaid ? "row-paid" : ""}>
                  <td>
                    <label className="status-control">
                      <input
                        type_="checkbox"
                        className="checkbox-control"
                        checked={isPaid}
                        onChange={_ => togglePaidMonth(row.month)}
                        ariaLabel={"Mark month " ++ Belt.Int.toString(row.month) ++ (isPaid ? " as unpaid" : " as paid")}
                      />
                      <span className={"status-badge " ++ (isPaid ? "paid" : "pending")}>
                        {React.string(isPaid ? "Paid" : "Pending")}
                      </span>
                    </label>
                  </td>
                  <td className="col-month"> {React.string(Belt.Int.toString(row.month))} </td>
                  <td> {React.string(formatCurrency(row.payment))} </td>
                  <td> {React.string(formatCurrency(row.interest))} </td>
                  <td> {React.string(formatCurrency(row.principal))} </td>
                  <td> {React.string(formatCurrency(row.balance))} </td>
                </tr>
              })
              ->React.array}
            </tbody>
          </table>
        </div>
      </div>
    </>
  }

  <div className=containerClass>
    <input
      id="loan-file-input"
      className="visually-hidden"
      type_="file"
      accept="application/json,.json"
      ref={ReactDOM.Ref.domRef(fileInputRef)}
      onChange=handleFileChange
      ariaLabel="Choose a loan calculator JSON file"
    />

    <div className="loan-calculator">
      <header className="calc-header">
        <h1 className="calc-title"> {React.string("Loan Calculator")} </h1>
        <div className="header-actions">
          <button type_="button" className="btn-action" onClick={_ => handleExport()} disabled={!allProfilesValid}>
            {React.string("Export Profiles")}
          </button>
          <button
            type_="button"
            className="btn-action"
            onClick={_ => switch fileInputRef.current->Nullable.toOption {
            | Some(input) => LoanPersistence.clickFileInput(input)
            | None => ()
            }}>
            {React.string("Import JSON")}
          </button>
          <button
            type_="button"
            className="btn-action"
            onClick={_ => setTheme(prev => prev == Oled ? Light : Oled)}
            ariaLabel={switch theme {
            | Oled => "Switch to light theme"
            | Light => "Switch to OLED theme"
            }}>
            {React.string(switch theme {
            | Oled => "OLED Black"
            | Light => "White"
            })}
          </button>
        </div>
      </header>

      <div className="feedback-message" role="status" ariaLive=#polite>
        {switch feedback {
        | Some(message) => React.string(message)
        | None => React.null
        }}
      </div>

      <section className={profilesExpanded ? "profiles-panel" : "profiles-panel profiles-panel-collapsed"}>
        <div className={profilesExpanded ? "profiles-header" : "profiles-header profiles-header-collapsed"}>
          <div className="profiles-heading">
            <button
              type_="button"
              className="profiles-toggle"
              ariaExpanded={profilesExpanded}
              ariaControls="loan-profiles-content"
              onClick={_ => setProfilesExpanded(prev => !prev)}>
              <span>
                <span className="eyebrow"> {React.string("Loan profiles")} </span>
                <span className="profile-title"> {React.string("Choose what you are tracking")} </span>
              </span>
              <span className="profiles-chevron" ariaHidden=true>
                {React.string(profilesExpanded ? "▴" : "▾")}
              </span>
            </button>
          </div>
          {profilesExpanded ? <div className="profile-actions">
            <label className="profile-picker">
              <span className="visually-hidden"> {React.string("Active loan profile")} </span>
              <select
                id="profile-select"
                className="input-control"
                value={Belt.Int.toString(activeProfileIndex)}
                onChange={event => {
                  switch Belt.Int.fromString(ReactEvent.Form.target(event)["value"]) {
                  | Some(index) => {
                    setActiveProfileIndex(_ => index)
                    clearFeedback()
                  }
                  | None => ()
                  }
                }}>
                {profiles
                ->Belt.Array.mapWithIndex((index, profile) =>
                  <option key={Belt.Int.toString(index)} value={Belt.Int.toString(index)}>
                    {React.string(profileOptionLabel(profile, index))}
                  </option>
                )
                ->React.array}
              </select>
            </label>
            <button type_="button" className="btn-action" onClick={_ => handleAddProfile()}>
              {React.string("New Profile")}
            </button>
            <button
              type_="button"
              className="btn-action"
              disabled={Belt.Array.length(profiles) <= 1}
              onClick={_ => handleDeleteProfile()}>
              {React.string("Delete")}
            </button>
          </div> : React.null}
        </div>
        {profilesExpanded ? <div id="loan-profiles-content" className="profile-fields">
          <label className="input-field">
            <span className="field-label"> {React.string("Profile Name")} </span>
            <input
              id="profile-name-input"
              className="input-control"
              type_="text"
              value={activeProfile.name}
              onChange={event => {
                let value = ReactEvent.Form.target(event)["value"]
                updateActiveProfile(profile => {...profile, name: value})
                clearFeedback()
              }}
            />
          </label>
          <label className="input-field">
            <span className="field-label"> {React.string("Purpose (Optional)")} </span>
            <input
              id="profile-purpose-input"
              className="input-control"
              type_="text"
              placeholder="e.g. Laptop purchase or friend loan"
              value={activeProfile.purpose}
              onChange={event => {
                let value = ReactEvent.Form.target(event)["value"]
                updateActiveProfile(profile => {...profile, purpose: value})
                clearFeedback()
              }}
            />
          </label>
        </div> : React.null}
      </section>

      <div className="inputs-grid">
        <label className="input-field">
          <span className="field-label"> {React.string("Planned Loan Amount (₹)")} </span>
          <input
            id="principal-input"
            className="input-control"
            type_="number"
            min="0.01"
            step=0.01
            inputMode="decimal"
            value=activeProfile.principalInput
            onChange={event => {
              let value = ReactEvent.Form.target(event)["value"]
              updateActiveProfile(profile => {
                // Keep the default single disbursement in sync with the planned
                // amount until the user has explicitly customized it.
                let disbursements = if Belt.Array.length(profile.disbursements) == 1 {
                  profile.disbursements->Belt.Array.map(disbursement =>
                    disbursement.amountInput == profile.principalInput
                      ? {...disbursement, amountInput: value}
                      : disbursement
                  )
                } else {
                  profile.disbursements
                }
                {...profile, principalInput: value, disbursements}
              })
              clearFeedback()
            }}
          />
        </label>

        <label className="input-field">
          <span className="field-label"> {React.string("Annual Rate (%)")} </span>
          <input
            id="annual-rate-input"
            className="input-control"
            type_="number"
            min="0"
            step=0.05
            inputMode="decimal"
            value=activeProfile.annualRateInput
            onChange={event => {
              let value = ReactEvent.Form.target(event)["value"]
              updateActiveProfile(profile => {...profile, annualRateInput: value})
              clearFeedback()
            }}
          />
        </label>

        <label className="input-field">
          <span className="field-label"> {React.string("Tenure (Months)")} </span>
          <input
            id="tenure-input"
            className="input-control"
            type_="number"
            min="1"
            max={Belt.Int.toString(LoanMath.maxTenureMonths)}
            step=1.0
            inputMode="numeric"
            value=activeProfile.tenureMonthsInput
            onChange={event => {
              let value = ReactEvent.Form.target(event)["value"]
              updateActiveProfile(profile => {...profile, tenureMonthsInput: value})
              clearFeedback()
            }}
          />
        </label>

        <div className="input-field">
          <span className="field-label">
            <label htmlFor="repayment-style-input"> {React.string("Repayment Style")} </label>
            <span className="tooltip-wrapper">
              <button
                type_="button"
                className="info-button"
                ariaLabel="Show repayment style explanations"
                ariaDescribedby="repayment-style-help">
                {React.string("i")}
              </button>
              <span id="repayment-style-help" className="tooltip-card" role="tooltip">
                <span className="tooltip-title"> {React.string("Which repayment style should I use?")} </span>
                <span className="tooltip-item">
                  <strong> {React.string("Flat Rate — simple personal loans: ")} </strong>
                  {React.string("Use this when a partner, friend, or family member agrees to fixed interest on the original loan amount. Principal and interest are split into equal monthly payments. Interest does not reduce as the balance falls, so this usually costs more than EMI at the same rate and term.")}
                </span>
                <span className="tooltip-item">
                  <strong> {React.string("EMI — bank-style loans: ")} </strong>
                  {React.string("Use this for a mortgage, vehicle loan, or personal loan when interest should be recalculated each month on the unpaid balance. The total payment is usually the same, but the interest part falls as principal is repaid. This usually costs less than Flat Rate at the same rate and term.")}
                </span>
                <span className="tooltip-item">
                  <strong> {React.string("Bullet — one big payment at the end: ")} </strong>
                  {React.string("Use this only when the borrower will return the full original principal on a known date. Pay interest each month, then repay the entire loan amount at the end. This fits short-term bridge loans or a loan backed by an expected lump sum.")}
                </span>
              </span>
            </span>
          </span>
          <select
            id="repayment-style-input"
            className="input-control"
            value={styleValue(activeProfile.style)}
            onChange={event => {
              let value = ReactEvent.Form.target(event)["value"]
              let nextStyle = switch LoanMath.repaymentStyleFromString(value) {
              | Some(nextStyle) => nextStyle
              | None => LoanMath.FlatRate
              }
              updateActiveProfile(profile => {...profile, style: nextStyle})
              clearFeedback()
            }}>
            <option value="flat"> {React.string("Flat Rate (Simple Interest)")} </option>
            <option value="emi"> {React.string("EMI (Reducing Balance)")} </option>
            <option value="bullet"> {React.string("Bullet / Lump Sum at Maturity")} </option>
          </select>
        </div>
      </div>

      <section className="disbursements-panel">
        <div className={fundingPlanExpanded
          ? "disbursements-header"
          : "disbursements-header disbursements-header-collapsed"}>
          <div>
            <button
              type_="button"
              className="funding-plan-toggle"
              ariaExpanded={fundingPlanExpanded}
              ariaControls="funding-plan-content"
              onClick={_ => setFundingPlanExpanded(prev => !prev)}>
              <span>
                <span className="eyebrow"> {React.string("Funding plan")} </span>
                <span className="profile-title"> {React.string("Money handed over")} </span>
              </span>
              <span className="funding-plan-chevron" ariaHidden=true>
                {React.string(fundingPlanExpanded ? "▴" : "▾")}
              </span>
            </button>
            {fundingPlanExpanded ? <p className="section-description">
              {React.string("Add each amount when it is actually given. Loan month 1 is the first repayment month. Interest and repayments begin for each tranche from its loan month.")}
            </p> : React.null}
          </div>
          {fundingPlanExpanded ? <button type_="button" className="btn-action" onClick={_ => handleAddDisbursement()}>
            {React.string("Add Disbursement")}
          </button> : React.null}
        </div>

        {fundingPlanExpanded ? <div id="funding-plan-content">
        <div className="funding-summary">
          {switch calculationResult {
          | Ok(calculation) => {
              let remainingCommitment = Math.max(
                calculation.plannedPrincipal -. calculation.disbursedPrincipal,
                0.0,
              )
              <>
                <div className="funding-stat">
                  <span> {React.string("Committed")} </span>
                  <strong> {React.string(formatCurrency(calculation.plannedPrincipal))} </strong>
                </div>
                <div className="funding-stat">
                  <span> {React.string("Handed over")} </span>
                  <strong> {React.string(formatCurrency(calculation.disbursedPrincipal))} </strong>
                </div>
                <div className="funding-stat funding-stat-highlight">
                  <span> {React.string("Still to hand over")} </span>
                  <strong> {React.string(formatCurrency(remainingCommitment))} </strong>
                </div>
              </>
            }
          | Error(_) =>
            <p className="funding-summary-error">
              {React.string("Complete the disbursement rows to see the funding summary.")}
            </p>
          }}
        </div>

        <div className="disbursements-list">
          {activeProfile.disbursements
          ->Belt.Array.mapWithIndex((index, disbursement) =>
            <div className="disbursement-row" key={Belt.Int.toString(index)}>
              <div className="disbursement-number" ariaHidden=true>
                {React.string(Belt.Int.toString(index + 1))}
              </div>
              <label className="input-field">
                <span className="field-label"> {React.string("Amount given (₹)")} </span>
                <input
                  className="input-control"
                  type_="number"
                  min="0.01"
                  step=0.01
                  inputMode="decimal"
                  value={disbursement.amountInput}
                  onChange={event => {
                    let value = ReactEvent.Form.target(event)["value"]
                    updateActiveDisbursement(index, row => {...row, amountInput: value})
                    clearFeedback()
                  }}
                />
              </label>
              <label className="input-field">
                <span className="field-label"> {React.string("Loan month")} </span>
                <input
                  className="input-control"
                  type_="number"
                  min="1"
                  max={Belt.Int.toString(LoanMath.maxTenureMonths)}
                  step=1.0
                  inputMode="numeric"
                  value={disbursement.monthInput}
                  onChange={event => {
                    let value = ReactEvent.Form.target(event)["value"]
                    updateActiveDisbursement(index, row => {...row, monthInput: value})
                    clearFeedback()
                  }}
                />
              </label>
              <button
                type_="button"
                className="btn-action disbursement-remove"
                disabled={Belt.Array.length(activeProfile.disbursements) <= 1}
                onClick={_ => handleRemoveDisbursement(index)}
                ariaLabel={"Remove disbursement " ++ Belt.Int.toString(index + 1)}>
                {React.string("Remove")}
              </button>
            </div>
          )
          ->React.array}
        </div>
        </div> : React.null}
      </section>

      {content}
    </div>
  </div>
}
