1. 데이터 수집

-> 데이터 수집 과정에서 이슈

-> 국민은행의 비정상적인 리뷰 수
증거: 3월 리뷰수 비교
OR 가능하면 이전 달 리뷰 수와 비교. 

-> 국민은행 리뷰이벤트 이슈 발생, 분석 제외.

편항된 결과가 있을 수도있고, 데이터가 너무 많아서 수집이 불가했다.

2. 분석 은행: 기업은행, 카카오뱅크, 산업은행, 농협은행, 신한은행, 우리은행 // 총 6개
3. 데이터 탐색. - str, 데이터 갯수, 결측치 없는 거 확인
   
   - 각 은행마다의 데이터 특징 설명.
4. 전체 데이터에서 샘플링 - 데이터 불균형 문제 해결
5. 각 은행 별 텍스트 마이닝, 워드클라우드
   - 평점 5점만 따로 워드 클라우드 
   
   - 평점 1점만 따로 워드 클라우드
   
   - 은행 평점 - 2021년/ 2020년 상/하, 2019년 상/하
     
     - 산업은행 처럼 데이터 갯수가 부족하면 능동적으로 하기. 
     
   - 강점/약점 정도 제시. 
   
     

코드 짜는 걸 분할

나는 워드클라우드 관련.

- 전체 워드클라우드 나오는 거
- 5점 짜리 따로
- 1점 짜리 따로

영민형

* 분기 별 나누도록.



기업: 2019년 5월 21일 , 7460개. 

카카오: 2018년 11월 20일, 8800개

농협: 2019년 1월 25일, 6920개 

신한: 2018년 12월 24일, 8280개

우리: 2019년 8월 16일, 6905개 - 2019년 6월까지 있으면.

----------------------------------------

산업은행: 2015년 9월 15일, 703개  - 능동적으로 보여주기



은행사별 디지털 계획은 이미 2018년 전후로 진행되고 있었기에 분류기준이 될 수 없음

(4)분기별 데이터 정제 -> 그냥 보려고 ..

코로나 이전 , 코로나 이후 -> 디지털 전환의 중요성이 더욱 증대되었기 때문

국내 코로나 첫 확진자 2020년 01월 19일 기준 정제

 

+ 리뷰 길이와 부정적 평가 간의 연관성 가설 보여주기도 가능할 듯

---

텍스트마이닝: 텍스트가 가지는 의미를 분석하는 것

or 비정형 덱스트 데이터에서 의미있는 정보를 찾아내는 기술

감성분석: 어떤 주제에 대한 주관적인 감정을 텍스트로부터 뽑아내는 분석.

제목: "은행 앱 리뷰 데이터 분석을 통한 인사이트 도출"

