package com.teenthofabud.laundromat.manager.access.permission.service;

import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.laundromat.manager.access.permission.data.PermissionException;
import com.teenthofabud.laundromat.manager.access.permission.data.PermissionForm;
import com.teenthofabud.laundromat.manager.access.permission.data.PermissionVo;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Set;

@Service
public interface PermissionService {

    public Set<PermissionVo> retrieveAllByNaturalOrdering();

    public PermissionVo retrieveDetailsById(long id) throws PermissionException;

    public List<PermissionVo> retrieveAllMatchingDetailsByResource(Long resourceId) throws PermissionException;
    public List<PermissionVo> retrieveAllMatchingDetailsByOperation(Long operationId) throws PermissionException;

    public Long createPermission(PermissionForm form) throws PermissionException;

    public void updatePermission(Long id, PermissionForm form) throws PermissionException;

    public void deletePermission(Long id) throws PermissionException;

    public void applyPatchOnPermission(Long id, List<PatchOperationForm> patches) throws PermissionException;

}
