# Data Validation

## Problem

초기 분석에서 특정 프로필 속성의 사용자당 평균이 운영 화면의 체감과 크게 달랐습니다.

수치가 계산되었다는 이유만으로 채택하지 않고 데이터 의미를 다시 확인했습니다.

## Validation Process

### 1. Sanity Check

먼저 결과가 서비스 운영 맥락에서 가능한 값인지 확인했습니다.

### 2. Source Semantics

비슷한 이름의 컬렉션이라도:
- 사용자가 등록한 속성
- 사용자가 행동으로 남긴 로그
- 추천/좋아요 관계

는 서로 다른 의미를 가질 수 있습니다.

처음 사용한 원천이 실제 측정하려는 개념과 다르다는 점을 확인했습니다.

### 3. Cross-check

사용자 프로필의 다른 필드와 운영 화면을 비교해 올바른 source를 다시 선정했습니다.

### 4. Recalculate

원천 변경 후 동일한 active-user population에 대해 다시 계산했습니다.

## Lesson

이 사례에서 중요한 것은 "평균값을 계산했다"가 아니라:

```text
Metric anomaly
→ Source inspection
→ Semantic mismatch found
→ Source corrected
→ Metric recalculated
```

의 검증 과정입니다.

제품 데이터에서는 컬럼명과 컬렉션명이 비슷해도 business meaning이 다를 수 있으므로, 값보다 먼저 이벤트/필드의 의미를 확인해야 합니다.
