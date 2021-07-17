package com.teenthofabud.laundromat.manager.access.operation.repository;

import com.teenthofabud.core.common.repository.TOABAdvancedEntityBaseRepository;
import com.teenthofabud.laundromat.manager.access.operation.data.OperationEntity;
import org.springframework.data.jpa.repository.Lock;
import org.springframework.stereotype.Repository;

import javax.persistence.LockModeType;


@Repository
public interface OperationRepository extends TOABAdvancedEntityBaseRepository<OperationEntity> {

    @Lock(LockModeType.PESSIMISTIC_WRITE)
    public OperationEntity save(OperationEntity entity);

}
