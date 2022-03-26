short_movies2 =  pd.read_csv('data/movie.csv', nrows=1000,
                        dtype={
                            'num_critic_for_reviews': np.float16,
                            'duration': np.float16,
                            'director_facebook_likes': np.float16,
                            'actor_3_facebook_likes': np.float16,
                            'actor_1_facebook_likes': np.float16,
                            'gross':np.float16,
                            'facenumber_in_poster':np.float16,
                            'num_user_for_reviews': np.float16,
                            'budget': np.float16,
                            'title_year': np.float16,
                            'actor_2_facebook_likes': np.float16,
                            'imdb_score': np.float16,
                            'aspect_ratio':np.float16,
                            'num_voted_users':np.int16})
                            
short_movies2.info()



# URL 에서 HTML 테이블 가져오기
url = 'https://ko.wikipedia.org/wiki/%EB%B2%A0%EB%84%A4%EB%94%95%ED%8A%B8_%EC%BB%B4%EB%B2%84%EB%B0%B0%EC%B9%98'
dfs = pd.read_html(url)
len(dfs)



