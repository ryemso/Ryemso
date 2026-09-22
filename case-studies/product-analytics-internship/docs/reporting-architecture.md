# Reporting Architecture

일부 비율 지표는 제품 분석 도구의 기본 aggregation만으로 정의를 맞추기 어려워, 다음 구조의 자동 리포트 방식을 검토했습니다.

```text
MongoDB
  ↓
PyMongo
  ↓
Pandas
  ↓
Metric calculation
  ↓
Matplotlib / CSV / PDF / Slack
```

## Intended Metrics

- Active User / profile completeness
- Weekly referral rate
- Like Rate
- Chat Request Rate
- selected payment / recommendation metrics

## Status

이 구조는 당시 **설계 및 검토**한 대안입니다.

실제 production scheduler, Slack bot, 배포 파이프라인을 완성했다고 주장하지 않습니다.
