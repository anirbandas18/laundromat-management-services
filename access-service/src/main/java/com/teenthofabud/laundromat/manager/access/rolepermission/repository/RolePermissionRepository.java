package com.teenthofabud.laundromat.manager.access.rolepermission.repository;

import com.teenthofabud.laundromat.manager.access.rolepermission.data.RolePermissionEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Lock;
import org.springframework.stereotype.Repository;

import javax.persistence.LockModeType;
import java.util.List;

@Repository
public interface RolePermissionRepository extends JpaRepository<RolePermissionEntity, Long> {

    @Lock(LockModeType.PESSIMISTIC_WRITE)
    public RolePermissionEntity save(RolePermissionEntity entity);

    @Lock(LockModeType.PESSIMISTIC_READ)
    public List<RolePermissionEntity> findByPermissionId(Long permissionId);

    @Lock(LockModeType.PESSIMISTIC_READ)
    public List<RolePermissionEntity> findByRoleId(Long roleId);

    @Lock(LockModeType.PESSIMISTIC_WRITE)
    public Boolean existsByPermissionId(Long permissionId);

    @Lock(LockModeType.PESSIMISTIC_WRITE)
    public Boolean existsByRoleId(Long roleId);

    @Lock(LockModeType.PESSIMISTIC_WRITE)
    public Boolean existsByPermissionIdAndRoleId(Long permissionId, Long roleId);

}
