type repaymentStyle =
  | FlatRate
  | Emi
  | Bullet

type scheduleRow = {
  month: int,
  payment: float,
  interest: float,
  principal: float,
  balance: float,
}

type loanInput = {
  principal: float,
  annualRate: float,
  tenureMonths: int,
}

type disbursement = {
  amount: float,
  month: int,
}

type stagedLoanInput = {
  plannedPrincipal: float,
  annualRate: float,
  tenureMonths: int,
  disbursements: array<disbursement>,
}

type calculation = {
  monthlyLabel: string,
  monthlyPayment: float,
  totalInterest: float,
  totalRepaid: float,
  plannedPrincipal: float,
  disbursedPrincipal: float,
  schedule: array<scheduleRow>,
}

type validationError =
  | InvalidPrincipal
  | InvalidAnnualRate
  | InvalidTenure
  | TenureTooLong
  | InvalidRepaymentStyle
  | InvalidDisbursement
  | DisbursementsExceedCommitment
  | CalculationOverflow

let maxTenureMonths = 1200

let defaultInput: loanInput = {
  principal: 300000.0,
  annualRate: 7.25,
  tenureMonths: 12,
}

let repaymentStyleToString = (style: repaymentStyle): string => switch style {
| FlatRate => "flat"
| Emi => "emi"
| Bullet => "bullet"
}

let repaymentStyleFromString = (value: string): option<repaymentStyle> => switch value {
| "flat" => Some(FlatRate)
| "emi" => Some(Emi)
| "bullet" => Some(Bullet)
| _ => None
}

let validationMessage = (error: validationError): string => switch error {
| InvalidPrincipal => "Enter a principal greater than zero."
| InvalidAnnualRate => "Enter an annual rate of zero or more."
| InvalidTenure => "Enter a whole-number tenure between 1 and 1,200 months."
| TenureTooLong => "Tenure cannot exceed 1,200 months."
| InvalidRepaymentStyle => "The imported repayment style is not supported."
| InvalidDisbursement => "Enter a positive amount and a valid loan month for every disbursement."
| DisbursementsExceedCommitment => "Disbursements cannot be greater than the planned loan amount."
| CalculationOverflow => "These values are too large to calculate safely."
}

let validateLoanInput = (input: loanInput): result<loanInput, validationError> => {
  if !Float.isFinite(input.principal) || input.principal <= 0.0 {
    Error(InvalidPrincipal)
  } else if !Float.isFinite(input.annualRate) || input.annualRate < 0.0 {
    Error(InvalidAnnualRate)
  } else if input.tenureMonths <= 0 {
    Error(InvalidTenure)
  } else if input.tenureMonths > maxTenureMonths {
    Error(TenureTooLong)
  } else {
    Ok(input)
  }
}

let decimalInputPattern = RegExp.fromString(
  "^[+-]?(?:\\d+(?:\\.\\d*)?|\\.\\d+)(?:[eE][+-]?\\d+)?$",
)

let parseStrictFloat = (value: string): option<float> => {
  let trimmed = String.trim(value)
  if trimmed == "" || !RegExp.test(decimalInputPattern, trimmed) {
    None
  } else {
    Belt.Float.fromString(trimmed)
  }
}

let parseStrictInt = (value: string): option<int> => switch parseStrictFloat(value) {
| Some(number) if Float.isFinite(number) && number >= -2147483648.0 && number <= 2147483647.0 => {
    let integer = Belt.Float.toInt(number)
    if Belt.Int.toFloat(integer) == number {
      Some(integer)
    } else {
      None
    }
  }
| _ => None
}

let parseLoanInput = (
  ~principalInput: string,
  ~annualRateInput: string,
  ~tenureMonthsInput: string,
): result<loanInput, validationError> => {
  switch (
    parseStrictFloat(principalInput),
    parseStrictFloat(annualRateInput),
    parseStrictInt(tenureMonthsInput),
  ) {
  | (Some(principal), Some(annualRate), Some(tenureMonths)) =>
    validateLoanInput({principal, annualRate, tenureMonths})
  | (None, _, _) => Error(InvalidPrincipal)
  | (_, None, _) => Error(InvalidAnnualRate)
  | (_, _, None) => Error(InvalidTenure)
  }
}

let totalDisbursed = (disbursements: array<disbursement>): float =>
  disbursements->Belt.Array.reduce(0.0, (total, disbursement) => total +. disbursement.amount)

let validateStagedLoanInput = (input: stagedLoanInput): result<stagedLoanInput, validationError> => {
  switch validateLoanInput({
    principal: input.plannedPrincipal,
    annualRate: input.annualRate,
    tenureMonths: input.tenureMonths,
  }) {
  | Error(error) => Error(error)
  | Ok(_) => {
      let hasInvalidDisbursement = input.disbursements->Belt.Array.some(disbursement =>
        !Float.isFinite(disbursement.amount) ||
        disbursement.amount <= 0.0 ||
        disbursement.month < 1 ||
        disbursement.month > input.tenureMonths
      )
      let disbursedPrincipal = totalDisbursed(input.disbursements)
      if Belt.Array.length(input.disbursements) == 0 || hasInvalidDisbursement {
        Error(InvalidDisbursement)
      } else if
        !Float.isFinite(disbursedPrincipal) ||
        disbursedPrincipal -. input.plannedPrincipal > 0.000001 {
        Error(DisbursementsExceedCommitment)
      } else {
        Ok(input)
      }
    }
  }
}

let parseStagedLoanInput = (
  ~plannedPrincipalInput: string,
  ~annualRateInput: string,
  ~tenureMonthsInput: string,
  ~disbursementInputs: array<(string, string)>,
): result<stagedLoanInput, validationError> => {
  switch (
    parseStrictFloat(plannedPrincipalInput),
    parseStrictFloat(annualRateInput),
    parseStrictInt(tenureMonthsInput),
  ) {
  | (Some(plannedPrincipal), Some(annualRate), Some(tenureMonths)) => {
      let stagedInput = disbursementInputs->Belt.Array.reduce(
        Ok([]),
        (parsed, (amountInput, monthInput)) => switch parsed {
        | Error(error) => Error(error)
        | Ok(disbursements) => switch (parseStrictFloat(amountInput), parseStrictInt(monthInput)) {
          | (Some(amount), Some(month)) => Ok(Belt.Array.concat(disbursements, [{amount, month}]))
          | _ => Error(InvalidDisbursement)
          }
        },
      )
      switch stagedInput {
      | Error(error) => Error(error)
      | Ok(disbursements) => validateStagedLoanInput({
          plannedPrincipal,
          annualRate,
          tenureMonths,
          disbursements,
        })
      }
    }
  | (None, _, _) => Error(InvalidPrincipal)
  | (_, None, _) => Error(InvalidAnnualRate)
  | (_, _, None) => Error(InvalidTenure)
  }
}

let calcEmi = (~principal: float, ~monthlyRate: float, ~months: int): float => {
  if months <= 0 {
    0.0
  } else if monthlyRate == 0.0 {
    principal /. Belt.Int.toFloat(months)
  } else {
    let n = Belt.Int.toFloat(months)
    let growthLog = Math.log1p(monthlyRate)
    let oneMinusDiscount = 0.0 -. Math.expm1(0.0 -. growthLog *. n)
    principal *. monthlyRate /. oneMinusDiscount
  }
}

let buildEmiSchedule = (~principal: float, ~monthlyRate: float, ~months: int, ~emi: float): array<scheduleRow> => {
  if months <= 0 {
    []
  } else if monthlyRate == 0.0 {
    let balance = ref(principal)
    Belt.Array.makeBy(months, i => {
      let isLastMonth = i == months - 1
      let principalPart = isLastMonth ? balance.contents : emi
      let actualPayment = isLastMonth ? principalPart : emi
      let nextBalance = isLastMonth ? 0.0 : Math.max(balance.contents -. principalPart, 0.0)
      balance := nextBalance
      {
        month: i + 1,
        payment: actualPayment,
        interest: 0.0,
        principal: principalPart,
        balance: nextBalance,
      }
    })
  } else {
    let growthLog = Math.log1p(monthlyRate)
    Belt.Array.makeBy(months, i => {
      let isLastMonth = i == months - 1
      let remainingMonths = months - i
      let remaining = Belt.Int.toFloat(remainingMonths)
      let oneMinusDiscount = 0.0 -. Math.expm1(0.0 -. growthLog *. remaining)
      let balance = emi *. oneMinusDiscount /. monthlyRate
      let interestPart = balance *. monthlyRate
      let discountFactor = Math.exp(0.0 -. growthLog *. remaining)
      let principalPart = isLastMonth ? balance : emi *. discountFactor
      let actualPayment = isLastMonth ? principalPart +. interestPart : emi
      let nextRemaining = remainingMonths - 1
      let nextBalance = if isLastMonth {
        0.0
      } else {
        let next = Belt.Int.toFloat(nextRemaining)
        let nextOneMinusDiscount = 0.0 -. Math.expm1(0.0 -. growthLog *. next)
        emi *. nextOneMinusDiscount /. monthlyRate
      }
      {
        month: i + 1,
        payment: actualPayment,
        interest: interestPart,
        principal: principalPart,
        balance: nextBalance,
      }
    })
  }
}

let buildFlatRateSchedule = (~principal: float, ~totalInterest: float, ~months: int): array<scheduleRow> => {
  if months <= 0 {
    []
  } else {
    let interestPerMonth = totalInterest /. Belt.Int.toFloat(months)
    let principalPerMonth = principal /. Belt.Int.toFloat(months)
    let balance = ref(principal)
    Belt.Array.makeBy(months, i => {
      let isLastMonth = i == months - 1
      let principalPart = isLastMonth ? balance.contents : principalPerMonth
      let nextBalance = isLastMonth ? 0.0 : Math.max(balance.contents -. principalPart, 0.0)
      balance := nextBalance
      {
        month: i + 1,
        payment: interestPerMonth +. principalPart,
        interest: interestPerMonth,
        principal: principalPart,
        balance: nextBalance,
      }
    })
  }
}

let buildBulletSchedule = (~principal: float, ~totalInterest: float, ~months: int): array<scheduleRow> => {
  if months <= 0 {
    []
  } else {
    let interestPerMonth = totalInterest /. Belt.Int.toFloat(months)
    Belt.Array.makeBy(months, i => {
      let isLastMonth = i == months - 1
      let principalPart = isLastMonth ? principal : 0.0
      let nextBalance = isLastMonth ? 0.0 : principal
      {
        month: i + 1,
        payment: interestPerMonth +. principalPart,
        interest: interestPerMonth,
        principal: principalPart,
        balance: nextBalance,
      }
    })
  }
}

let buildStagedFlatSchedule = (~input: stagedLoanInput): array<scheduleRow> => {
  let balance = ref(0.0)
  Belt.Array.makeBy(input.tenureMonths, i => {
    let month = i + 1
    let newlyDisbursed = input.disbursements->Belt.Array.reduce(0.0, (total, disbursement) =>
      disbursement.month == month ? total +. disbursement.amount : total
    )
    let interest = input.disbursements->Belt.Array.reduce(0.0, (total, disbursement) =>
      if month >= disbursement.month {
        total +. disbursement.amount *. input.annualRate /. 12.0 /. 100.0
      } else {
        total
      }
    )
    let principal = input.disbursements->Belt.Array.reduce(0.0, (total, disbursement) =>
      if month >= disbursement.month {
        let remainingMonths = input.tenureMonths - disbursement.month + 1
        total +. disbursement.amount /. Belt.Int.toFloat(remainingMonths)
      } else {
        total
      }
    )
    balance := balance.contents +. newlyDisbursed -. principal
    let nextBalance = month == input.tenureMonths ? 0.0 : Math.max(balance.contents, 0.0)
    balance := nextBalance
    {
      month,
      payment: interest +. principal,
      interest,
      principal,
      balance: nextBalance,
    }
  })
}

let stagedEmiParts = (
  ~disbursement: disbursement,
  ~month: int,
  ~annualRate: float,
  ~tenureMonths: int,
): (float, float, float) => {
  let trancheMonths = tenureMonths - disbursement.month + 1
  let monthlyRate = annualRate /. 12.0 /. 100.0
  let emi = calcEmi(
    ~principal=disbursement.amount,
    ~monthlyRate,
    ~months=trancheMonths,
  )
  if monthlyRate == 0.0 {
    let principal = disbursement.amount /. Belt.Int.toFloat(trancheMonths)
    (principal, 0.0, principal)
  } else {
    let remainingMonths = tenureMonths - month + 1
    let remaining = Belt.Int.toFloat(remainingMonths)
    let growthLog = Math.log1p(monthlyRate)
    let oneMinusDiscount = 0.0 -. Math.expm1(0.0 -. growthLog *. remaining)
    let balance = emi *. oneMinusDiscount /. monthlyRate
    let interest = balance *. monthlyRate
    let discountFactor = Math.exp(0.0 -. growthLog *. remaining)
    let principal = month == tenureMonths ? balance : emi *. discountFactor
    let payment = month == tenureMonths ? principal +. interest : emi
    (payment, interest, principal)
  }
}

let buildStagedEmiSchedule = (~input: stagedLoanInput): array<scheduleRow> => {
  let balance = ref(0.0)
  Belt.Array.makeBy(input.tenureMonths, i => {
    let month = i + 1
    let newlyDisbursed = input.disbursements->Belt.Array.reduce(0.0, (total, disbursement) =>
      disbursement.month == month ? total +. disbursement.amount : total
    )
    let (payment, interest, principal) = input.disbursements->Belt.Array.reduce(
      (0.0, 0.0, 0.0),
      ((paymentTotal, interestTotal, principalTotal), disbursement) =>
        if month >= disbursement.month {
          let (payment, interest, principal) = stagedEmiParts(
            ~disbursement,
            ~month,
            ~annualRate=input.annualRate,
            ~tenureMonths=input.tenureMonths,
          )
          (
            paymentTotal +. payment,
            interestTotal +. interest,
            principalTotal +. principal,
          )
        } else {
          (paymentTotal, interestTotal, principalTotal)
        },
    )
    balance := balance.contents +. newlyDisbursed -. principal
    let nextBalance = month == input.tenureMonths ? 0.0 : Math.max(balance.contents, 0.0)
    balance := nextBalance
    {
      month,
      payment,
      interest,
      principal,
      balance: nextBalance,
    }
  })
}

let buildStagedBulletSchedule = (~input: stagedLoanInput): array<scheduleRow> => {
  let balance = ref(0.0)
  let disbursedPrincipal = totalDisbursed(input.disbursements)
  Belt.Array.makeBy(input.tenureMonths, i => {
    let month = i + 1
    let newlyDisbursed = input.disbursements->Belt.Array.reduce(0.0, (total, disbursement) =>
      disbursement.month == month ? total +. disbursement.amount : total
    )
    let interest = input.disbursements->Belt.Array.reduce(0.0, (total, disbursement) =>
      if month >= disbursement.month {
        total +. disbursement.amount *. input.annualRate /. 12.0 /. 100.0
      } else {
        total
      }
    )
    let principal = month == input.tenureMonths ? disbursedPrincipal : 0.0
    balance := balance.contents +. newlyDisbursed -. principal
    let nextBalance = month == input.tenureMonths ? 0.0 : Math.max(balance.contents, 0.0)
    balance := nextBalance
    {
      month,
      payment: interest +. principal,
      interest,
      principal,
      balance: nextBalance,
    }
  })
}

let rowIsFinite = (row: scheduleRow): bool =>
  Float.isFinite(row.payment) &&
  Float.isFinite(row.interest) &&
  Float.isFinite(row.principal) &&
  Float.isFinite(row.balance)

let calculationIsFinite = (value: calculation): bool =>
  Float.isFinite(value.monthlyPayment) &&
  Float.isFinite(value.totalInterest) &&
  Float.isFinite(value.totalRepaid) &&
  Float.isFinite(value.plannedPrincipal) &&
  Float.isFinite(value.disbursedPrincipal) &&
  Belt.Array.every(value.schedule, rowIsFinite)

let calculate = (~input: loanInput, ~style: repaymentStyle): result<calculation, validationError> => {
  switch validateLoanInput(input) {
  | Error(error) => Error(error)
  | Ok(input) =>
    let monthsAsFloat = Belt.Int.toFloat(input.tenureMonths)
    let monthlyRate = input.annualRate /. 12.0 /. 100.0
    let calculation = switch style {
    | FlatRate =>
      let totalInterest = input.principal *. input.annualRate *. (monthsAsFloat /. 12.0) /. 100.0
      let totalRepaid = input.principal +. totalInterest
      let monthlyPayment = totalRepaid /. monthsAsFloat
      {
        monthlyLabel: "Monthly payment",
        monthlyPayment,
        totalInterest,
        totalRepaid,
        plannedPrincipal: input.principal,
        disbursedPrincipal: input.principal,
        schedule: buildFlatRateSchedule(~principal=input.principal, ~totalInterest, ~months=input.tenureMonths),
      }
    | Emi =>
      let monthlyPayment = calcEmi(~principal=input.principal, ~monthlyRate, ~months=input.tenureMonths)
      let totalRepaid = monthlyPayment *. monthsAsFloat
      let totalInterest = Math.max(totalRepaid -. input.principal, 0.0)
      {
        monthlyLabel: "Monthly payment",
        monthlyPayment,
        totalInterest,
        totalRepaid,
        plannedPrincipal: input.principal,
        disbursedPrincipal: input.principal,
        schedule: buildEmiSchedule(
          ~principal=input.principal,
          ~monthlyRate,
          ~months=input.tenureMonths,
          ~emi=monthlyPayment,
        ),
      }
    | Bullet =>
      let totalInterest = input.principal *. input.annualRate *. (monthsAsFloat /. 12.0) /. 100.0
      let totalRepaid = input.principal +. totalInterest
      let monthlyPayment = totalInterest /. monthsAsFloat
      {
        monthlyLabel: "Monthly interest (lump sum at end)",
        monthlyPayment,
        totalInterest,
        totalRepaid,
        plannedPrincipal: input.principal,
        disbursedPrincipal: input.principal,
        schedule: buildBulletSchedule(~principal=input.principal, ~totalInterest, ~months=input.tenureMonths),
      }
    }
    if calculationIsFinite(calculation) {
      Ok(calculation)
    } else {
      Error(CalculationOverflow)
    }
  }
}

let summarizeSchedule = (schedule: array<scheduleRow>): (float, float) =>
  schedule->Belt.Array.reduce((0.0, 0.0), (totals, row) => {
    let (interest, repaid) = totals
    (interest +. row.interest, repaid +. row.payment)
  })

let firstSchedulePayment = (schedule: array<scheduleRow>): float => switch Belt.Array.get(schedule, 0) {
| Some(row) => row.payment
| None => 0.0
}

let calculateStaged = (
  ~input: stagedLoanInput,
  ~style: repaymentStyle,
): result<calculation, validationError> => {
  switch validateStagedLoanInput(input) {
  | Error(error) => Error(error)
  | Ok(input) => {
      let schedule = switch style {
      | FlatRate => buildStagedFlatSchedule(~input)
      | Emi => buildStagedEmiSchedule(~input)
      | Bullet => buildStagedBulletSchedule(~input)
      }
      let (totalInterest, totalRepaid) = summarizeSchedule(schedule)
      let calculation = {
        monthlyLabel: "Month 1 payment",
        monthlyPayment: firstSchedulePayment(schedule),
        totalInterest,
        totalRepaid,
        plannedPrincipal: input.plannedPrincipal,
        disbursedPrincipal: totalDisbursed(input.disbursements),
        schedule,
      }
      if calculationIsFinite(calculation) {
        Ok(calculation)
      } else {
        Error(CalculationOverflow)
      }
    }
  }
}

let normalizePaidMonths = (~months: int, ~paidMonths: array<int>): array<int> =>
  paidMonths->Belt.Array.reduce([], (normalized, month) => {
    if month < 1 || month > months || Belt.Array.some(normalized, existing => existing == month) {
      normalized
    } else {
      Belt.Array.concat(normalized, [month])
    }
  })

let allPaidMonths = (months: int): array<int> =>
  if months <= 0 {
    []
  } else {
    Belt.Array.makeBy(months, month => month + 1)
  }
