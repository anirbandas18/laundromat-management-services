package com.teenthofabud.laundromat.manager.access.permission.repository;

import com.teenthofabud.laundromat.manager.access.permission.data.PermissionEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Lock;
import org.springframework.stereotype.Repository;

import javax.persistence.LockModeType;
import java.util.List;

@Repository
public interface PermissionRepository extends JpaRepository<PermissionEntity, Long> {

    @Lock(LockModeType.PESSIMISTIC_WRITE)
    public PermissionEntity save(PermissionEntity entity);

    @Lock(LockModeType.PESSIMISTIC_READ)
    public List<PermissionEntity> findByResourceId(Long resourceId);

    @Lock(LockModeType.PESSIMISTIC_READ)
    public List<PermissionEntity> findByOperationId(Long operationId);

    @Lock(LockModeType.PESSIMISTIC_WRITE)
    public Boolean existsByResourceId(Long resourceId);

    @Lock(LockModeType.PESSIMISTIC_WRITE)
    public Boolean existsByOperationId(Long operationId);

    @Lock(LockModeType.PESSIMISTIC_WRITE)
    public Boolean existsByResourceIdAndOperationId(Long resourceId, Long operationId);

}
