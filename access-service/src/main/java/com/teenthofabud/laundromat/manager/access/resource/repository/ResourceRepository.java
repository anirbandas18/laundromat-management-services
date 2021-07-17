package com.teenthofabud.laundromat.manager.access.resource.repository;

import com.teenthofabud.core.common.repository.TOABAdvancedEntityBaseRepository;
import com.teenthofabud.laundromat.manager.access.resource.data.ResourceEntity;
import org.springframework.data.jpa.repository.Lock;
import org.springframework.stereotype.Repository;

import javax.persistence.LockModeType;


@Repository
public interface ResourceRepository extends TOABAdvancedEntityBaseRepository<ResourceEntity> {

    @Lock(LockModeType.PESSIMISTIC_WRITE)
    public ResourceEntity save(ResourceEntity entity);

}
