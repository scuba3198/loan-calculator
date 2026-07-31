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

type calculation = {
  monthlyLabel: string,
  monthlyPayment: float,
  totalInterest: float,
  totalRepaid: float,
  schedule: array<scheduleRow>,
}

type validationError =
  | InvalidPrincipal
  | InvalidAnnualRate
  | InvalidTenure
  | TenureTooLong
  | InvalidRepaymentStyle
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

let rowIsFinite = (row: scheduleRow): bool =>
  Float.isFinite(row.payment) &&
  Float.isFinite(row.interest) &&
  Float.isFinite(row.principal) &&
  Float.isFinite(row.balance)

let calculationIsFinite = (value: calculation): bool =>
  Float.isFinite(value.monthlyPayment) &&
  Float.isFinite(value.totalInterest) &&
  Float.isFinite(value.totalRepaid) &&
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
