# Recommendation Analysis

## Question

추천 시스템이 특정 사용자에게 과도하게 노출을 집중시키는지, 그리고 실제 추천 가능한 후보 pool이 충분한지를 점검했습니다.

## Exposure Analysis

추천 로그에서 노출된 profile id 배열을 풀어 사용자별 exposure count를 계산했습니다.

분석 흐름:

```text
Recommendation log
→ unwind exposed profile ids
→ aggregate exposure count by exposed user
→ join user attributes
→ compare attribute buckets vs exposure
```

이 분석은 상관관계를 보는 것이며, 특정 사용자 특성이 노출을 "원인"으로 만들었다고 단정하지 않았습니다.

## Runway

최근 노출된 사용자를 제외한 후보 pool을 계산해 추천 소진 가능성을 확인했습니다.

```text
Available Candidate Pool
= Eligible candidates
- recently shown candidates
- invalid / unavailable candidates
```

이후 일별 소개량과 비교해 후보 pool의 상대적 여유를 확인했습니다.

## Operational Use

Runway가 낮다면 다음을 점검할 수 있습니다.

- 추천 조건이 지나치게 좁은지
- 특정 사용자군에 노출이 집중되는지
- 반복 소개를 제외했을 때 실제 pool이 작은지
- 선호 조건 또는 매칭 조건이 병목을 만드는지

## Limitation

공개 문서에서는 회사 내부 수치와 사용자 구분 기준을 제거했습니다. 이 문서는 분석 프레임과 aggregation 사고방식만 보여줍니다.
