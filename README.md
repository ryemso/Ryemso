# 김동현 | Data / Product Analyst

```text
┌─ ~/data-lab
│
├─ asking better questions
├─ testing assumptions
├─ building stronger evidence
│
└─ still iterating_
```

실제 서비스 사용자 행동 로그를 기반으로 **지표를 정의하고, 데이터 원천을 검증하고, 분석 결과를 제품·사업 의사결정으로 연결하는 데이터 분석가**를 지향합니다.

ML/DL 프로젝트 경험도 보유하고 있지만, 현재 가장 자신 있게 설명할 수 있는 대표 경험은 **실제 서비스 로그를 다룬 Product Analytics 업무**입니다.

- **Product Analytics**: SQL, Python, MongoDB, Amplitude, Tableau
- **Data Validation**: 지표 정의 · Unique User 기준 · 원천 데이터 검증 · 분석 단위 점검
- **ML / DL**: Scikit-learn, XGBoost, LightGBM, CatBoost, TensorFlow/Keras
- **Focus**: Raw Logs → Metric Definition → Validation → Analysis → Decision Support

## Representative Experience

### [DARE · Product Analytics Internship](./case-studies/product-analytics-internship/README.md)

실제 서비스의 사용자 행동·추천·결제·초대 로그를 다루며 **지표 정의 → 데이터 검증 → 분석 → 의사결정 지원**까지 수행했습니다.

단순히 쿼리 결과를 전달하는 것이 아니라:

- 동일한 이름의 지표라도 **이벤트·기간·Unique User 기준을 먼저 정의**
- 운영 맥락과 맞지 않는 값은 **원천 데이터와 필드 의미를 다시 검증**
- Active User, Profile Completeness, Referral, Payment, Like/Chat Request 등 **사용자 단위 지표 설계**
- 추천 로그를 사용자 단위로 재구성해 **노출 편중과 후보군 커버리지 점검**
- Amplitude에서 **Retention / Cohort / Rolling Window** 분석
- UI 기본 aggregation과 맞지 않는 지표는 **MongoDB → Python/Pandas 계산 구조**로 보완 검토

회사 데이터, 내부 식별자, 운영 수치와 내부 쿼리는 공개하지 않고, 분석 구조와 문제 해결 방식만 익명화해 정리했습니다.

**MongoDB Aggregation · Python · Pandas · Amplitude · Product Metrics · Data Validation**

> 대표 경험에서 가장 크게 배운 것은 **숫자를 만드는 것보다, 그 숫자가 어떤 데이터와 기준에서 만들어졌는지 설명할 수 있어야 한다**는 점입니다.

## Selected Projects

### [Olist E-commerce Analytics](https://github.com/ryemso/olist-ecommerce-analytics)
주문·결제·고객·상품·리뷰 데이터를 결합하며 n:n join 중복을 검증하고, **Seller 확보 · 고객 유지 · 배송 경험**을 분석했습니다.  
관측 데이터의 상관·회귀 결과를 인과효과로 과장하지 않고 비즈니스 우선순위로 연결했습니다.

**Python · Pandas · Tableau · Statistics · E-commerce Analytics**

### [Heat Demand Forecasting](https://github.com/ryemso/heat-demand-forecasting)
기상 데이터 기반 시간대별 열수요 예측 프로젝트입니다.  
BiLSTM, CNN-LSTM, Attention 계열 구조와 시퀀스 생성·스케일링·검증 경계 문제를 점검해 **검증 RMSE 21.7 → 17.2**를 기록했습니다.

**Python · TensorFlow/Keras · LSTM · CNN · Attention · Time Series**

### [Hanwoo Grade Prediction](https://github.com/ryemso/Hanwoo_ranke)
한우 개체·혈통·농장/지역·기상 데이터를 결합해 `LAST_GRADE` 16개 등급을 예측했습니다.  
train에만 존재하는 도축 후 판정 변수의 누수 가능성을 제거하고, 실제 test에서 사용할 수 있는 정보만으로 재설계해 **Public Macro-F1 0.219**를 기록했습니다.

**Python · CatBoost · XGBoost · Feature Engineering · Multi-class Classification**

### [Cognitive Impairment Prediction](https://github.com/ryemso/cognitive-impairment-prediction)
50세 이상 인구의 인지장애 경험 여부를 예측한 불균형 분류 프로젝트입니다.  
False Negative 비용을 고려해 Recall을 핵심 지표로 두고 모델 비교, Stacking, Optuna, Threshold 조정을 실험했습니다.

**Python · Scikit-learn · XGBoost · LightGBM · CatBoost · Optuna**

### [LendingClub Credit Risk Modeling](https://github.com/ryemso/lendingclub-credit-risk)
대출 상환 실패 가능성을 분류하고 클래스 불균형 처리와 여러 모델을 비교했습니다.  
과거 notebook을 다시 검토하며 초기 **target leakage**를 확인했고, 이를 제거한 공개용 실험 흐름과 pipeline을 재구성했습니다.

**Python · Imbalanced-learn · XGBoost · LightGBM · SHAP**

### [The Liquidation of Penny](https://github.com/ryemso/The-Liquidation-of-Penny)
금융·시장 개념을 전투와 성장 시스템으로 구현한 플레이 가능한 2D 액션 로그라이트 브라우저 프로토타입입니다.  
게임 시스템뿐 아니라 **이벤트 로깅 · run 단위 플레이 분석 · 자동 회귀 테스트**까지 함께 설계했습니다.

**JavaScript · Game Systems · Event Logging · Analytics**

## How I Work

```text
Problem / Business Question
        ↓
Metric Definition
        ↓
Data Source Validation
        ↓
Analysis / Modeling
        ↓
Result Verification
        ↓
Decision Support
```

프로젝트를 완료했다는 사실보다 **다른 사람이 문제 정의, 데이터, 실험, 결과를 다시 따라갈 수 있는 상태**로 남기는 것을 중요하게 생각합니다.

## Portfolio

- [Product Data Analyst](https://kimsportpolio.netlify.app/?ver=analyst)
- [AI / Machine Learning](https://kimsportpolio.netlify.app/?ver=ai)
- [Business / Growth Analytics](https://kimsportpolio.netlify.app/?ver=strategy)

## Contact

- GitHub: [github.com/ryemso](https://github.com/ryemso)
- LinkedIn: [동현 김](https://www.linkedin.com/in/%EB%8F%99%ED%98%84-%EA%B9%80-898ba4348)
- Email: qt0177@gmail.com
