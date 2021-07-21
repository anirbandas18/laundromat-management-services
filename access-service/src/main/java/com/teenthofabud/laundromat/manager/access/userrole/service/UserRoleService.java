package com.teenthofabud.laundromat.manager.access.userrole.service;

import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.laundromat.manager.access.userrole.data.UserRoleException;
import com.teenthofabud.laundromat.manager.access.userrole.data.UserRoleForm;
import com.teenthofabud.laundromat.manager.access.userrole.data.UserRoleVo;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Set;

@Service
public interface UserRoleService {

    public Set<UserRoleVo> retrieveAllByNaturalOrdering();

    public UserRoleVo retrieveDetailsById(Long id) throws UserRoleException;

    public List<UserRoleVo> retrieveAllMatchingDetailsByUserType(Long userTypeId) throws UserRoleException;

    public List<UserRoleVo> retrieveAllMatchingDetailsByRole(Long roleId) throws UserRoleException;

    public Long createUserRole(UserRoleForm form) throws UserRoleException;

    public void updateUserRole(Long id, UserRoleForm form) throws UserRoleException;

    public void deleteUserRole(Long id) throws UserRoleException;

    public void applyPatchOnUserRole(Long id, List<PatchOperationForm> patches) throws UserRoleException;

}
