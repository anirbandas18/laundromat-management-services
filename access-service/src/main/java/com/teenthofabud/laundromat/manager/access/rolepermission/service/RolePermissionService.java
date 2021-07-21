package com.teenthofabud.laundromat.manager.access.rolepermission.service;

import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.laundromat.manager.access.rolepermission.data.RolePermissionException;
import com.teenthofabud.laundromat.manager.access.rolepermission.data.RolePermissionForm;
import com.teenthofabud.laundromat.manager.access.rolepermission.data.RolePermissionVo;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Set;

@Service
public interface RolePermissionService {

    public Set<RolePermissionVo> retrieveAllByNaturalOrdering();

    public RolePermissionVo retrieveDetailsById(Long id) throws RolePermissionException;

    public List<RolePermissionVo> retrieveAllMatchingDetailsByPermission(Long permissionId) throws RolePermissionException;

    public List<RolePermissionVo> retrieveAllMatchingDetailsByRole(Long roleId) throws RolePermissionException;

    public Long createRolePermission(RolePermissionForm form) throws RolePermissionException;

    public void updateRolePermission(Long id, RolePermissionForm form) throws RolePermissionException;

    public void deleteRolePermission(Long id) throws RolePermissionException;

    public void applyPatchOnRolePermission(Long id, List<PatchOperationForm> patches) throws RolePermissionException;

}
