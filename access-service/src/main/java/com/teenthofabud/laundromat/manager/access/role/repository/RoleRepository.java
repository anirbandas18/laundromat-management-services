package com.teenthofabud.laundromat.manager.access.role.repository;

import com.teenthofabud.core.common.repository.TOABAdvancedEntityBaseRepository;
import com.teenthofabud.laundromat.manager.access.role.data.RoleEntity;
import org.springframework.data.jpa.repository.Lock;
import org.springframework.stereotype.Repository;

import javax.persistence.LockModeType;


@Repository
public interface RoleRepository extends TOABAdvancedEntityBaseRepository<RoleEntity> {

    @Lock(LockModeType.PESSIMISTIC_WRITE)
    public RoleEntity save(RoleEntity entity);

}
