package com.teenthofabud.laundromat.manager.access.securityquestion.repository;

import com.teenthofabud.core.common.repository.TOABAdvancedEntityBaseRepository;
import com.teenthofabud.laundromat.manager.access.securityquestion.data.SecurityQuestionEntity;
import org.springframework.data.jpa.repository.Lock;
import org.springframework.stereotype.Repository;

import javax.persistence.LockModeType;


@Repository
public interface SecurityQuestionRepository extends TOABAdvancedEntityBaseRepository<SecurityQuestionEntity> {

    @Lock(LockModeType.PESSIMISTIC_WRITE)
    public SecurityQuestionEntity save(SecurityQuestionEntity entity);

}
