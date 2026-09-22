# Data Analysis Case Studies

실제 프로젝트 산출물과 Google Drive 원본을 기준으로 정리한 분석 사례입니다.

## 1. Olist E-commerce Analytics

[Case Study 보기](./olist/README.md)

브라질 Olist 전자상거래 데이터를 이용해 Seller 확보, 고객 유지, 배송 지연 문제를 분석했습니다.

- 중복 발생 원인 검증 및 전처리
- Seller 수와 주문량 관계 분석
- 카테고리 수요·공급 / HHI 분석
- 고객 재구매와 Review Score 분석
- 배송기간과 만족도 관계 분석
- Tableau 대시보드 기반 의사결정 구조

> 현재 연결된 Drive에서 확인된 원본은 발표자료 PDF입니다. 분석 코드 파일은 확인되지 않아 발표자료에 근거한 Case Study로만 정리했습니다.

## 2. LendingClub Credit Risk Modeling

[Case Study 보기](./lendingclub/README.md) · [Refactored Code](./lendingclub/modeling_pipeline.py)

LendingClub 대출 데이터를 이용해 상환 실패 가능성을 분류하고 불균형 처리와 모델 비교를 수행했습니다.

- 결측치 처리 및 파생변수
- SMOTE / RandomUnderSampler / NearMiss / Tomek Links 비교
- KNN / Logistic Regression / Decision Tree / Random Forest / SVM / XGBoost / LightGBM
- RandomizedSearchCV
- Precision / Recall / F1 / ROC AUC / PR AUC / KS
- SHAP 기반 해석

> 코드는 Google Drive의 원본 Colab notebook을 포트폴리오 검토용으로 재구성한 버전입니다.
