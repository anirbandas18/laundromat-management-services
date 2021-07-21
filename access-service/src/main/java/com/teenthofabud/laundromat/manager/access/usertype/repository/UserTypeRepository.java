package com.teenthofabud.laundromat.manager.access.usertype.repository;

import com.teenthofabud.core.common.repository.TOABAdvancedEntityBaseRepository;
import com.teenthofabud.laundromat.manager.access.usertype.data.UserTypeEntity;
import org.springframework.data.jpa.repository.Lock;
import org.springframework.stereotype.Repository;

import javax.persistence.LockModeType;


@Repository
public interface UserTypeRepository extends TOABAdvancedEntityBaseRepository<UserTypeEntity> {

    @Lock(LockModeType.PESSIMISTIC_WRITE)
    public UserTypeEntity save(UserTypeEntity entity);

}
