import test from "node:test";
import assert from "node:assert/strict";
import * as LoanMath from "../src/LoanMath.bs.js";
import * as LoanPersistence from "../src/LoanPersistence.bs.js";

const unwrapOk = (result) => {
  assert.equal(result.TAG, "Ok");
  return result._0;
};

const unwrapError = (result) => {
  assert.equal(result.TAG, "Error");
  return result._0;
};

const parseValid = (principal, annualRate, tenureMonths) =>
  unwrapOk(LoanMath.parseLoanInput(String(principal), String(annualRate), String(tenureMonths)));

const calculate = (style, input = parseValid(300000, 7.25, 12)) =>
  unwrapOk(LoanMath.calculate(input, LoanMath.repaymentStyleFromString(style)));

test("flat-rate calculation balances the schedule", () => {
  const result = calculate("flat");

  assert.equal(result.monthlyPayment, 26812.5);
  assert.equal(result.totalInterest, 21750);
  assert.equal(result.totalRepaid, 321750);
  assert.equal(result.schedule.length, 12);
  assert.equal(result.schedule.at(-1).balance, 0);
  assert.equal(result.schedule.reduce((sum, row) => sum + row.principal, 0), 300000);
});

test("EMI handles zero interest and reducing balance correctly", () => {
  const zeroInterest = calculate("emi", parseValid(120000, 0, 12));
  assert.equal(zeroInterest.monthlyPayment, 10000);
  assert.equal(zeroInterest.totalInterest, 0);
  assert.equal(zeroInterest.schedule.at(-1).balance, 0);

  const standard = calculate("emi");
  assert.ok(Math.abs(standard.monthlyPayment - 25992.611640106086) < 0.000001);
  assert.ok(standard.schedule[0].interest > standard.schedule.at(-1).interest);
  assert.equal(standard.schedule.at(-1).balance, 0);
});

test("bullet repayment charges interest monthly and principal at maturity", () => {
  const result = calculate("bullet");

  assert.equal(result.monthlyPayment, 1812.5);
  assert.equal(result.schedule[0].principal, 0);
  assert.equal(result.schedule.at(-1).principal, 300000);
  assert.equal(result.schedule.at(-1).payment, 301812.5);
  assert.equal(result.schedule.at(-1).balance, 0);
});

test("input validation rejects malformed and unsafe values", () => {
  assert.equal(unwrapError(LoanMath.parseLoanInput("", "7.25", "12")), "InvalidPrincipal");
  assert.equal(unwrapError(LoanMath.parseLoanInput("300000", "-1", "12")), "InvalidAnnualRate");
  assert.equal(unwrapError(LoanMath.parseLoanInput("300000", "7.25", "0")), "InvalidTenure");
  assert.equal(unwrapError(LoanMath.parseLoanInput("300000", "7.25", "1201")), "TenureTooLong");
  assert.equal(unwrapError(LoanMath.parseLoanInput("Infinity", "7.25", "12")), "InvalidPrincipal");
});

test("paid months are bounded and deduplicated", () => {
  assert.deepEqual(
    LoanMath.normalizePaidMonths(12, [1, 1, 0, 13, -2, 3]),
    [1, 3],
  );
  assert.deepEqual(LoanMath.allPaidMonths(3), [1, 2, 3]);
  assert.deepEqual(LoanMath.allPaidMonths(0), []);
});

test("JSON persistence round-trips and normalizes payment history", () => {
  const json = LoanPersistence.encode(
    300000,
    7.25,
    12,
    LoanMath.repaymentStyleFromString("emi"),
    [1, 1, 0, 13],
    [2],
    [3],
  );
  const saved = unwrapOk(LoanPersistence.decode(json));

  assert.equal(saved.principal, 300000);
  assert.equal(saved.annualRate, 7.25);
  assert.equal(saved.tenureMonths, 12);
  assert.equal(saved.style, "Emi");
  assert.deepEqual(saved.flatPaidMonths, [1]);
  assert.deepEqual(saved.emiPaidMonths, [2]);
  assert.deepEqual(saved.bulletPaidMonths, [3]);
});

test("JSON persistence supports legacy histories and rejects invalid files", () => {
  const legacy = LoanPersistence.decode(
    '{"principal":1000,"annualRate":0,"tenureMonths":3,"style":"bullet","paidMonths":[1,3,99]}',
  );
  const saved = unwrapOk(legacy);
  assert.deepEqual(saved.flatPaidMonths, []);
  assert.deepEqual(saved.emiPaidMonths, []);
  assert.deepEqual(saved.bulletPaidMonths, [1, 3]);

  assert.equal(unwrapError(LoanPersistence.decode("not json")), "InvalidJson");
  assert.equal(
    unwrapError(LoanPersistence.decode('{"principal":1000,"annualRate":0,"tenureMonths":3,"style":"unknown"}')),
    "InvalidFormat",
  );
  assert.equal(
    unwrapError(LoanPersistence.decode('{"principal":1000,"annualRate":0,"tenureMonths":3,"paidMonths":{}}')),
    "InvalidFormat",
  );
});
