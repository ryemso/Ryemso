# Metric Definition

서비스 내부 명칭과 실제 값은 비공개로 두고, 지표 정의 방식만 정리했습니다.

## Active User

활동 기준이 되는 이벤트를 먼저 선정한 뒤 기간 내 unique user를 계산합니다.

```text
Active Users
= COUNT(DISTINCT user_id)
  WHERE qualifying_event occurred within analysis window
```

핵심은 이벤트 하나를 임의로 고르는 것이 아니라, 여러 activity log를 비교해 "서비스를 실제 사용했다"고 해석 가능한 기준을 정하는 것입니다.

## Referral Rate

초대 건수를 그대로 사용하지 않고 unique inviter를 기준으로 정의했습니다.

```text
Referral Rate
= Unique users who sent at least one referral
  / Total users in the same period
```

기간별 비교 시 분자와 분모의 기간 기준을 동일하게 유지합니다.

## Paying / Repeat Paying User

```text
Paying User
= Unique users with at least one valid payment

Repeat Paying User
= Unique users with two or more valid payments
```

결제 transaction 수와 결제 user 수를 구분합니다.

## Recommendation Metrics

```text
Like Rate
= Unique like actions / Unique profile exposures

Chat Request Rate
= Unique chat-request actions / Unique profile exposures
```

단순 document count보다 사용자 또는 노출 단위의 중복 제거 기준이 중요합니다.

## Candidate-pool Runway

추천 대상 pool과 최근 노출 이력을 비교해 앞으로 소개 가능한 후보 수를 추정합니다.

고려 요소:
- 성별 / 선호 조건
- 중복 노출 제거
- 최근 노출 이력
- 추천 가능 상태
- 사용자별 후보 pool 크기

이 지표는 "추천 후보가 언제 소진될 수 있는가"를 운영 관점에서 점검하기 위한 분석 지표입니다.
