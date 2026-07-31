type savedState = {
  principal: float,
  annualRate: float,
  tenureMonths: int,
  style: LoanMath.repaymentStyle,
  flatPaidMonths: array<int>,
  emiPaidMonths: array<int>,
  bulletPaidMonths: array<int>,
}

type importError =
  | InvalidJson
  | InvalidFormat
  | InvalidLoan

@module("./LoanFile.js")
external downloadJson: (string, string) => unit = "downloadJson"

@module("./LoanFile.js")
external readFileAsText: ('a, string => unit, string => unit) => unit = "readFileAsText"

@module("./LoanFile.js")
external clearFileInput: 'a => unit = "clearFileInput"

@module("./LoanFile.js")
external clickFileInput: 'a => unit = "clickFileInput"

let jsonField = (object: dict<JSON.t>, key: string): option<JSON.t> => Dict.get(object, key)

let decodeFloat = (object: dict<JSON.t>, key: string): option<float> =>
  switch jsonField(object, key) {
  | Some(value) => JSON.Decode.float(value)
  | None => None
  }

let decodeInt = (value: JSON.t): option<int> =>
  switch JSON.Decode.float(value) {
  | Some(number) =>
    if Float.isFinite(number) && number >= -2147483648.0 && number <= 2147483647.0 {
      let integer = Belt.Float.toInt(number)
      if Belt.Int.toFloat(integer) == number {
        Some(integer)
      } else {
        None
      }
    } else {
      None
    }
  | None => None
  }

let decodeIntArray = (value: JSON.t): option<array<int>> =>
  switch JSON.Decode.array(value) {
  | Some(values) =>
    Some(values->Belt.Array.reduce([], (decoded, value) => {
      if Belt.Array.length(decoded) >= LoanMath.maxTenureMonths {
        decoded
      } else {
        switch decodeInt(value) {
        | Some(number) => Belt.Array.concat(decoded, [number])
        | None => decoded
        }
      }
    }))
  | None => None
  }

let decodeOptionalIntArray = (object: dict<JSON.t>, key: string): option<array<int>> =>
  switch jsonField(object, key) {
  | None => Some([])
  | Some(value) => decodeIntArray(value)
  }

let decodeStyle = (object: dict<JSON.t>): option<LoanMath.repaymentStyle> =>
  switch jsonField(object, "style") {
  | None => Some(LoanMath.FlatRate)
  | Some(value) => switch JSON.Decode.string(value) {
    | Some(style) => LoanMath.repaymentStyleFromString(style)
    | None => None
    }
  }

let decodePaidMonths = (
  ~root: dict<JSON.t>,
  ~style: LoanMath.repaymentStyle,
  ~tenureMonths: int,
): result<(array<int>, array<int>, array<int>), importError> => {
  switch jsonField(root, "paidMonthsByStyle") {
  | Some(value) => switch JSON.Decode.object(value) {
    | Some(byStyle) =>
      switch (
        decodeOptionalIntArray(byStyle, "flat"),
        decodeOptionalIntArray(byStyle, "emi"),
        decodeOptionalIntArray(byStyle, "bullet"),
      ) {
      | (Some(flat), Some(emi), Some(bullet)) =>
        Ok((
          LoanMath.normalizePaidMonths(~months=tenureMonths, ~paidMonths=flat),
          LoanMath.normalizePaidMonths(~months=tenureMonths, ~paidMonths=emi),
          LoanMath.normalizePaidMonths(~months=tenureMonths, ~paidMonths=bullet),
        ))
      | _ => Error(InvalidFormat)
      }
    | None => Error(InvalidFormat)
    }
  | None =>
    switch switch jsonField(root, "paidMonths") {
    | Some(value) => decodeIntArray(value)
    | None => Some([])
    } {
    | None => Error(InvalidFormat)
    | Some(legacyMonths) =>
      let normalized = LoanMath.normalizePaidMonths(~months=tenureMonths, ~paidMonths=legacyMonths)
      switch style {
      | LoanMath.FlatRate => Ok((normalized, [], []))
      | LoanMath.Emi => Ok(([], normalized, []))
      | LoanMath.Bullet => Ok(([], [], normalized))
      }
    }
  }
}

let decode = (content: string): result<savedState, importError> => {
  try {
    let root = JSON.parseOrThrow(content)
    switch JSON.Decode.object(root) {
    | None => Error(InvalidFormat)
    | Some(root) =>
      switch (
        decodeFloat(root, "principal"),
        decodeFloat(root, "annualRate"),
        jsonField(root, "tenureMonths"),
        decodeStyle(root),
      ) {
      | (Some(principal), Some(annualRate), Some(tenureValue), Some(style)) =>
        switch decodeInt(tenureValue) {
        | None => Error(InvalidLoan)
        | Some(tenureMonths) =>
          switch LoanMath.validateLoanInput({principal, annualRate, tenureMonths}) {
          | Error(_) => Error(InvalidLoan)
          | Ok(_) => switch decodePaidMonths(~root, ~style, ~tenureMonths) {
            | Error(error) => Error(error)
            | Ok((flatPaidMonths, emiPaidMonths, bulletPaidMonths)) =>
              Ok({
                principal,
                annualRate,
                tenureMonths,
                style,
                flatPaidMonths,
                emiPaidMonths,
                bulletPaidMonths,
              })
            }
          }
        }
      | _ => Error(InvalidFormat)
      }
    }
  } catch {
  | JsExn(_) => Error(InvalidJson)
  }
}

let encode = (
  ~principal: float,
  ~annualRate: float,
  ~tenureMonths: int,
  ~style: LoanMath.repaymentStyle,
  ~flatPaidMonths: array<int>,
  ~emiPaidMonths: array<int>,
  ~bulletPaidMonths: array<int>,
): string => {
  let normalizedFlat = LoanMath.normalizePaidMonths(~months=tenureMonths, ~paidMonths=flatPaidMonths)
  let normalizedEmi = LoanMath.normalizePaidMonths(~months=tenureMonths, ~paidMonths=emiPaidMonths)
  let normalizedBullet = LoanMath.normalizePaidMonths(~months=tenureMonths, ~paidMonths=bulletPaidMonths)
  let paidMonthsByStyle = Dict.fromArray([
    ("flat", JSON.Encode.intArray(normalizedFlat)),
    ("emi", JSON.Encode.intArray(normalizedEmi)),
    ("bullet", JSON.Encode.intArray(normalizedBullet)),
  ])->JSON.Encode.object
  Dict.fromArray([
    ("version", JSON.Encode.int(2)),
    ("principal", JSON.Encode.float(principal)),
    ("annualRate", JSON.Encode.float(annualRate)),
    ("tenureMonths", JSON.Encode.int(tenureMonths)),
    ("style", JSON.Encode.string(LoanMath.repaymentStyleToString(style))),
    ("paidMonthsByStyle", paidMonthsByStyle),
  ])->JSON.Encode.object->JSON.stringify
}

let importErrorMessage = (error: importError): string => switch error {
| InvalidJson => "The selected file is not valid JSON."
| InvalidFormat => "The selected file does not match the loan calculator format."
| InvalidLoan => "The imported loan values are outside the supported range."
}

let download = (content: string): unit => downloadJson("loan-calculator.json", content)
