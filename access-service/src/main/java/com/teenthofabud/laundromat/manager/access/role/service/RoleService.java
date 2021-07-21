package com.teenthofabud.laundromat.manager.access.role.service;

import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.laundromat.manager.access.role.data.RoleException;
import com.teenthofabud.laundromat.manager.access.role.data.RoleForm;
import com.teenthofabud.laundromat.manager.access.role.data.RoleVo;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Set;

@Service
public interface RoleService {

    public Set<RoleVo> retrieveAllByNaturalOrdering();

    public RoleVo retrieveDetailsById(long id) throws RoleException;

    public List<RoleVo> retrieveAllMatchingDetailsByName(String name) throws RoleException;

    public Long createRole(RoleForm form) throws RoleException;

    public void updateRole(Long id, RoleForm form) throws RoleException;

    public void deleteRole(Long id) throws RoleException;

    public void applyPatchOnRole(Long id, List<PatchOperationForm> patches) throws RoleException;

}
