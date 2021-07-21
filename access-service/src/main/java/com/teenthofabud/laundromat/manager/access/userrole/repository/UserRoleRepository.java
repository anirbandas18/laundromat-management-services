package com.teenthofabud.laundromat.manager.access.userrole.repository;

import com.teenthofabud.laundromat.manager.access.userrole.data.UserRoleEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Lock;
import org.springframework.stereotype.Repository;

import javax.persistence.LockModeType;
import java.util.List;

@Repository
public interface UserRoleRepository extends JpaRepository<UserRoleEntity, Long> {

    @Lock(LockModeType.PESSIMISTIC_WRITE)
    public UserRoleEntity save(UserRoleEntity entity);

    @Lock(LockModeType.PESSIMISTIC_READ)
    public List<UserRoleEntity> findByUserTypeId(Long userTypeId);

    @Lock(LockModeType.PESSIMISTIC_READ)
    public List<UserRoleEntity> findByRoleId(Long roleId);

    @Lock(LockModeType.PESSIMISTIC_WRITE)
    public Boolean existsByUserTypeId(Long userTypeId);

    @Lock(LockModeType.PESSIMISTIC_WRITE)
    public Boolean existsByRoleId(Long roleId);

    @Lock(LockModeType.PESSIMISTIC_WRITE)
    public Boolean existsByUserTypeIdAndRoleId(Long userTypeId, Long roleId);

}
