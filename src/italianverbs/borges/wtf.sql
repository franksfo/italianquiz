SELECT target.serialized::text AS target,target.surface
	                                        FROM expression AS target

	                                  INNER JOIN expression AS source
	                                          ON (source.structure->'synsem'->'sem') @> (target.structure->'synsem'->'sem')
	                                         AND (source.language='en')

	                                       WHERE target.language='it'
	                                         AND (target.structure @> '{"synsem":{"subcat":[]}}')
