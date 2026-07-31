%%raw(`import "./LoanCalculator.css"`)

type themeMode =
  | Oled
  | Light

@send external toLocaleString: (float, string) => string = "toLocaleString"

let formatCurrency = (value: float): string => {
  let rounded = Math.round(value)
  "₹" ++ toLocaleString(rounded, "en-IN")
}

let styleValue = (style: LoanMath.repaymentStyle): string =>
  LoanMath.repaymentStyleToString(style)

@react.component
let make = () => {
  let (principalInput, setPrincipalInput) = React.useState(() => "300000")
  let (annualRateInput, setAnnualRateInput) = React.useState(() => "7.25")
  let (tenureMonthsInput, setTenureMonthsInput) = React.useState(() => "12")

  let (style, setStyle) = React.useState(() => LoanMath.FlatRate)
  let (theme, setTheme) = React.useState(() => Oled)

  let (flatPaidMonths, setFlatPaidMonths) = React.useState(() => [])
  let (emiPaidMonths, setEmiPaidMonths) = React.useState(() => [])
  let (bulletPaidMonths, setBulletPaidMonths) = React.useState(() => [])
  let (feedback, setFeedback) = React.useState(() => None)

  let fileInputRef = React.useRef(Nullable.null)

  let parsedInput = LoanMath.parseLoanInput(
    ~principalInput,
    ~annualRateInput,
    ~tenureMonthsInput,
  )
  let calculationResult = switch parsedInput {
  | Ok(input) => LoanMath.calculate(~input, ~style)
  | Error(error) => Error(error)
  }
  let currentInput = switch parsedInput {
  | Ok(input) => input
  | Error(_) => LoanMath.defaultInput
  }

  let activePaidMonths = switch style {
  | LoanMath.FlatRate => flatPaidMonths
  | LoanMath.Emi => emiPaidMonths
  | LoanMath.Bullet => bulletPaidMonths
  }
  let normalizedActivePaidMonths = LoanMath.normalizePaidMonths(
    ~months=currentInput.tenureMonths,
    ~paidMonths=activePaidMonths,
  )

  let setActivePaidMonths = (update: array<int> => array<int>) => {
    let normalizedUpdate = prev =>
      LoanMath.normalizePaidMonths(
        ~months=currentInput.tenureMonths,
        ~paidMonths=update(prev),
      )
    switch style {
    | LoanMath.FlatRate => setFlatPaidMonths(normalizedUpdate)
    | LoanMath.Emi => setEmiPaidMonths(normalizedUpdate)
    | LoanMath.Bullet => setBulletPaidMonths(normalizedUpdate)
    }
  }

  let clearFeedback = () => setFeedback(_ => None)

  let togglePaidMonth = (month: int) => {
    setActivePaidMonths(prev => {
      let normalized = LoanMath.normalizePaidMonths(
        ~months=currentInput.tenureMonths,
        ~paidMonths=prev,
      )
      if Belt.Array.some(normalized, existing => existing == month) {
        Belt.Array.keep(normalized, existing => existing != month)
      } else {
        Belt.Array.concat(normalized, [month])
      }
    })
  }

  let handleExport = () => switch calculationResult {
  | Ok(_) =>
    let json = LoanPersistence.encode(
      ~principal=currentInput.principal,
      ~annualRate=currentInput.annualRate,
      ~tenureMonths=currentInput.tenureMonths,
      ~style,
      ~flatPaidMonths,
      ~emiPaidMonths,
      ~bulletPaidMonths,
    )
    LoanPersistence.download(json)
    setFeedback(_ => Some("Loan data exported."))
  | Error(_) => ()
  }

  let handleFileChange = (event: ReactEvent.Form.t) => {
    let target = ReactEvent.Form.target(event)
    let files = target["files"]
    if !Nullable.isNullable(files) && files["length"] > 0 {
      let file = files[0]
      LoanPersistence.clearFileInput(target)
      LoanPersistence.readFileAsText(
        file,
        content => switch LoanPersistence.decode(content) {
        | Ok(saved) => {
            setPrincipalInput(_ => Belt.Float.toString(saved.principal))
            setAnnualRateInput(_ => Belt.Float.toString(saved.annualRate))
            setTenureMonthsInput(_ => Belt.Int.toString(saved.tenureMonths))
            setStyle(_ => saved.style)
            setFlatPaidMonths(_ => saved.flatPaidMonths)
            setEmiPaidMonths(_ => saved.emiPaidMonths)
            setBulletPaidMonths(_ => saved.bulletPaidMonths)
            setFeedback(_ => Some("Loan data imported."))
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
        <div className="stat-card">
          <p className="stat-label"> {React.string(calculation.monthlyLabel)} </p>
          <p className="stat-value"> {React.string(formatCurrency(calculation.monthlyPayment))} </p>
        </div>
        <div className="stat-card">
          <p className="stat-label"> {React.string("Total Interest")} </p>
          <p className="stat-value"> {React.string(formatCurrency(calculation.totalInterest))} </p>
        </div>
        <div className="stat-card">
          <p className="stat-label"> {React.string("Total Repaid")} </p>
          <p className="stat-value"> {React.string(formatCurrency(calculation.totalRepaid))} </p>
        </div>
        <div className="stat-card">
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
          <button type_="button" className="btn-action" onClick={_ => handleExport()} disabled={switch calculationResult {
          | Ok(_) => false
          | Error(_) => true
          }}>
            {React.string("Export JSON")}
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

      <div className="inputs-grid">
        <label className="input-field">
          <span className="field-label"> {React.string("Principal (₹)")} </span>
          <input
            id="principal-input"
            className="input-control"
            type_="number"
            min="0.01"
            step=0.01
            inputMode="decimal"
            value=principalInput
            onChange={event => {
              setPrincipalInput(_ => ReactEvent.Form.target(event)["value"])
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
            value=annualRateInput
            onChange={event => {
              setAnnualRateInput(_ => ReactEvent.Form.target(event)["value"])
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
            value=tenureMonthsInput
            onChange={event => {
              setTenureMonthsInput(_ => ReactEvent.Form.target(event)["value"])
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
                <span className="tooltip-title"> {React.string("Repayment Style Guide")} </span>
                <span className="tooltip-item">
                  <strong> {React.string("Flat Rate (Simple Interest): ")} </strong>
                  {React.string("Equal monthly installments combining principal and flat simple interest. Best for personal or vehicle financing.")}
                </span>
                <span className="tooltip-item">
                  <strong> {React.string("EMI (Reducing Balance): ")} </strong>
                  {React.string("Equal monthly installments where interest decreases over time as principal amortizes. Standard for mortgages & bank loans.")}
                </span>
                <span className="tooltip-item">
                  <strong> {React.string("Bullet / Lump Sum: ")} </strong>
                  {React.string("Pay interest monthly during tenure, then repay the full principal lump sum at final maturity. Best for bridge/peer loans.")}
                </span>
              </span>
            </span>
          </span>
          <select
            id="repayment-style-input"
            className="input-control"
            value={styleValue(style)}
            onChange={event => {
              let value = ReactEvent.Form.target(event)["value"]
              setStyle(_ => switch LoanMath.repaymentStyleFromString(value) {
              | Some(nextStyle) => nextStyle
              | None => LoanMath.FlatRate
              })
              clearFeedback()
            }}>
            <option value="flat"> {React.string("Flat Rate (Simple Interest)")} </option>
            <option value="emi"> {React.string("EMI (Reducing Balance)")} </option>
            <option value="bullet"> {React.string("Bullet / Lump Sum at Maturity")} </option>
          </select>
        </div>
      </div>

      {content}
    </div>
  </div>
}
