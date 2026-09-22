# Product Analytics Internship Case Study

실제 서비스의 사용자 행동·추천·결제·초대 로그를 다루며 **지표 정의 → 데이터 검증 → 분석 → 의사결정 지원**까지 수행한 경험을 공개 가능한 범위에서 정리한 Case Study입니다.

> 회사 데이터, 내부 식별자, 원본 컬렉션명, 운영 수치, 내부 쿼리는 공개하지 않습니다. 아래 내용은 분석 구조와 문제 해결 방식만 익명화해 정리했습니다.

## Role

- 서비스 로그 구조 확인
- 사용자 단위 지표 정의
- 잘못된 데이터 원천 검증
- 추천 노출 및 후보군 커버리지 분석
- 결제·재결제·초대 행동 분석
- Amplitude Retention / Cohort 분석
- MongoDB → Python 리포팅 구조 검토

## Analysis Flow

### 1. Define

동일한 "활성 사용자"라도 방문·프로필 조회·기능 사용 등 이벤트마다 사용자 수가 달랐습니다.

단순히 가장 큰 숫자를 사용하는 대신:
- 이벤트 의미
- 기록 안정성
- 분석 목적
- 기간 일관성

을 비교해 활동 기준을 정리했습니다.

### 2. Validate

처음 계산한 프로필 속성 평균이 실제 운영 화면과 크게 달랐습니다.

값 자체를 그대로 사용하지 않고:
1. 사용한 컬렉션의 의미 재확인
2. 다른 사용자 프로필 필드와 비교
3. 운영 화면 sanity check
4. 원천 데이터 수정 후 재계산

순서로 검증했습니다.

[Data Validation 문서](./docs/data-validation.md)

### 3. Measure

다음과 같은 사용자 단위 지표를 정의했습니다.

- Active User
- Profile Completeness
- Referral Rate
- Paying User / Repeat Paying User
- Like Rate
- Chat Request Rate
- Recommendation Exposure
- Candidate-pool Runway

[Metric Definition 문서](./docs/metric-definition.md)

### 4. Diagnose

추천 분석에서는 단순 추천 횟수보다:
- 누가 얼마나 노출되는지
- 사용자 특성과 노출량이 어떤 관계인지
- 후보군이 얼마나 빨리 소진되는지
- 반복 노출을 제외한 실제 가용 pool이 충분한지

를 함께 점검했습니다.

[Recommendation Analysis 문서](./docs/recommendation-analysis.md)

### 5. Operate

Amplitude에서 Retention, Cohort, Rolling Window를 사용해 사용자 행동을 확인했습니다.

다만 unique-user 기반 비율이나 복합 분모가 필요한 지표는 UI 기본 aggregation과 정확히 맞지 않을 수 있어, MongoDB → Python/Pandas로 직접 계산하는 리포트 구조도 검토했습니다.

> 이 자동 리포트는 설계·검토 단계였으며 production 배포 완료로 표현하지 않습니다.

## Public Example

실제 운영 collection과 field를 공개하지 않기 위해 아래에는 익명화한 MongoDB aggregation 예시만 제공합니다.

[Anonymized Aggregation Example](./examples/anonymized_aggregation.js)

## What This Case Demonstrates

- 로그 구조를 먼저 확인하는 습관
- 지표의 분자·분모·unique 기준 정의
- 이상한 숫자를 발견했을 때 원천까지 역추적하는 검증 과정
- 사용자 행동 로그를 서비스 문제와 연결하는 분석
- 제품 분석 도구와 raw log 계산을 병행하는 방식

## Tech Stack

**MongoDB Aggregation · Python · Pandas · Amplitude · Product Metrics**
