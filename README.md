# Identifying the Impact of Inflation Expectations

## Description

This project exploits cross-sectional heterogeneity to credibly measure the pass-through from inflation expectations to inflation. 

### Abstract

Individuals form inflation expectations differently based on their demographics and locations. Consequently, different demographic groups respond differently to sectoral price changes, a fact that we exploit to identify the inflationary impact of expectations. Our instrument combines national expectations of specific groups with these groups' share in regional populations. We find that a one-percentage-point rise in the expected rate of inflation increases (regional) inflation by 60 basis points. Interestingly, long-run expectations -- say, over 5 to 10 years -- don't seem to matter much. The estimates are most robust for a particular demographic: younger, married individuals holding at least a high school diploma. Their expectations mainly influence the prices of non-durable goods.

### [Full Paper](https://william-branch.github.io/groupexpects/docs/main.pdf)

## Instructions for Reproducing This Project

1. **Clone the Repository:**
    ```bash
    git clone https://github.com/your_username/your_repository.git
    ```

2. **Navigate to the Project Directory:**
    ```bash
    cd your_repository
    ```

3. **Set up the `.env` file:**
    - Create a `.env` file in the root directory.
    - Copy the contents from `.env.example` into your `.env`.
    - Add your own FRED API key:
        ```text
        FRED_API_KEY="your_api_key_here"
        ```

4. **Run Make:**
    ```bash
    make
    ```

## Important Notes

- You'll need a FRED API key to run this project. Obtain it from [FRED's website](https://research.stlouisfed.org/fred2/) and put it in the `.env` file.



## Contributors

- [Bill Branch](https://williambranch.org)

