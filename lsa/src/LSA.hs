
termFrequency document term = length (filter (==t) document)

compress k m = u_k <> sigma_k <> v_k where
	(u,sigma,v) = fullSVD m			       
	sigma_k = (takeColumns k . takeRows k) sigma	
	u_k = takeColumns k u				
	v_k = takeRows k $ trans v			

collapseDimensions k m = v_k where
        (u,sigma,v) = fullSVD m                        
        v_k = takeRows k $ trans v                      

