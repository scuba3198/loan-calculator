# Loan Calculator

A modern, fast, and minimalist loan EMI & interest calculator web application built with **ReScript**, **React**, and **Vite**.

![OLED Theme](https://img.shields.io/badge/Theme-OLED%20Black%20%2F%20White-000000?style=for-the-badge)
![ReScript](https://img.shields.io/badge/ReScript-v12-red?style=for-the-badge&logo=rescript)
![React](https://img.shields.io/badge/React-v19-blue?style=for-the-badge&logo=react)
![Vite](https://img.shields.io/badge/Vite-v8-646CFF?style=for-the-badge&logo=vite)

> **Live Demo**: [https://scuba3198.github.io/loan-calculator/](https://scuba3198.github.io/loan-calculator/)

---

## Features

- **3 Repayment Modes**:
  - **Flat Rate (Simple Interest)**: Equal monthly installments combining principal and flat simple interest ($\frac{P}{n} + \frac{I}{n}$).
  - **EMI (Reducing Balance)**: Standard amortized monthly payments where interest decreases over time as principal is paid off.
  - **Bullet Repayment (Lump Sum at Maturity)**: Interest-only monthly payments during tenure, with 100% of principal $P$ paid at maturity.
- **OLED Black / White Minimalist Aesthetic**: High-contrast OLED dark mode (`#000000`) and crisp light mode toggle with thin hairline dividers.
- **Payment Done Tracking**: Mark individual months as paid with visual status badges, strike-through styling, and a live progress tracking bar.
- **Per-Style Payment Scoping**: Payment completion history is independently tracked for each repayment style.
- **Staged Disbursements**: Separate the planned loan commitment from the amounts actually handed over, record each tranche's loan month, and calculate interest only from when that tranche is funded.
- **Named Loan Profiles**: Track separate loans such as a purchase EMI, a business loan from a friend, or a bullet loan, each with its own purpose, repayment style, and payment history.
- **JSON Import & Export**: Export all loan profiles and payment progress to `.json` files. Older single-loan exports can still be imported.
- **Indian Locale Formatting (`en-IN`)**: All monetary figures are formatted with standard Indian comma grouping (e.g., `₹3,00,000`).
- **Fully Mobile Responsive**: Touch-friendly controls, fluid responsive grid, and horizontal scrollable amortization schedule table.

---

## Tech Stack

- **Language**: [ReScript 12](https://rescript-lang.org/) (Type-safe functional language compiling to clean JavaScript)
- **UI Library**: [React 19](https://react.dev/) (`@rescript/react`)
- **Bundler & Dev Server**: [Vite 8](https://vitejs.dev/)
- **Styling**: Vanilla CSS with CSS custom variables

---

## Getting Started

### Prerequisites

- [Node.js](https://nodejs.org/) (v18+)
- `npm` or `pnpm`

### Installation & Local Development

1. Clone the repository:
   ```bash
   git clone https://github.com/scuba3198/loan-calculator.git
   cd loan-calculator
   ```

2. Install dependencies:
   ```bash
   npm install
   ```

3. Build ReScript and start dev server:
   ```bash
   npm run dev
   ```

4. Run domain and regression tests:
   ```bash
   npm test
   ```

5. Build production bundle:
   ```bash
   npm run build
   ```

### Input limits

The calculator accepts positive planned loan amounts, non-negative annual rates, and tenures from 1 to 1,200 whole-number months. Each disbursement must be positive, fall within the loan tenure, and remain within the planned commitment. Invalid input is rejected instead of being silently converted to zero. Imported payment histories and disbursement rows are validated before use.

---

## Repository & License

Created and maintained by [@scuba3198](https://github.com/scuba3198).
