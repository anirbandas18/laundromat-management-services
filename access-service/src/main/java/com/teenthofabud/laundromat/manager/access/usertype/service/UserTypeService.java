package com.teenthofabud.laundromat.manager.access.usertype.service;

import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.laundromat.manager.access.usertype.data.UserTypeException;
import com.teenthofabud.laundromat.manager.access.usertype.data.UserTypeForm;
import com.teenthofabud.laundromat.manager.access.usertype.data.UserTypeVo;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Set;

@Service
public interface UserTypeService {

    public Set<UserTypeVo> retrieveAllByNaturalOrdering();

    public UserTypeVo retrieveDetailsById(long id) throws UserTypeException;

    public List<UserTypeVo> retrieveAllMatchingDetailsByName(String name) throws UserTypeException;

    public Long createUserType(UserTypeForm form) throws UserTypeException;

    public void updateUserType(Long id, UserTypeForm form) throws UserTypeException;

    public void deleteUserType(Long id) throws UserTypeException;

    public void applyPatchOnUserType(Long id, List<PatchOperationForm> patches) throws UserTypeException;

}
