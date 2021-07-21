package com.teenthofabud.laundromat.manager.access.userrole.mapper;

import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.core.common.mapper.DualChannelMapper;
import com.teenthofabud.laundromat.manager.access.error.AccessErrorCode;
import com.teenthofabud.laundromat.manager.access.role.data.RoleEntity;
import com.teenthofabud.laundromat.manager.access.role.data.RoleException;
import com.teenthofabud.laundromat.manager.access.role.data.RoleVo;
import com.teenthofabud.laundromat.manager.access.role.repository.RoleRepository;
import com.teenthofabud.laundromat.manager.access.role.service.RoleService;
import com.teenthofabud.laundromat.manager.access.userrole.data.UserRoleEntity;
import com.teenthofabud.laundromat.manager.access.userrole.data.UserRoleException;
import com.teenthofabud.laundromat.manager.access.userrole.data.UserRoleForm;
import com.teenthofabud.laundromat.manager.access.userrole.data.UserRoleMessageTemplate;
import com.teenthofabud.laundromat.manager.access.usertype.data.UserTypeEntity;
import com.teenthofabud.laundromat.manager.access.usertype.data.UserTypeException;
import com.teenthofabud.laundromat.manager.access.usertype.data.UserTypeVo;
import com.teenthofabud.laundromat.manager.access.usertype.repository.UserTypeRepository;
import com.teenthofabud.laundromat.manager.access.usertype.service.UserTypeService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.Optional;

@Component
@Slf4j
public class UserRoleForm2EntityMapper implements DualChannelMapper<UserRoleEntity, UserRoleForm> {

    private UserTypeService userTypeService;
    private RoleService roleService;
    private UserTypeRepository userTypeRepository;
    private RoleRepository roleRepository;

    @Autowired
    public void setUserTypeService(UserTypeService userTypeService) {
        this.userTypeService = userTypeService;
    }

    @Autowired
    public void setRoleService(RoleService roleService) {
        this.roleService = roleService;
    }

    @Autowired
    public void setUserTypeRepository(UserTypeRepository userTypeRepository) {
        this.userTypeRepository = userTypeRepository;
    }

    @Autowired
    public void setRoleRepository(RoleRepository roleRepository) {
        this.roleRepository = roleRepository;
    }

    @Override
    public Optional<UserRoleEntity> compareAndMap(UserRoleEntity actualEntity, UserRoleForm form) throws TOABBaseException {
        UserRoleEntity expectedEntity = new UserRoleEntity();
        boolean changeSW = false;
        // direct copy
        expectedEntity.setId(actualEntity.getId());
        log.debug("Directly copying UserRoleEntity.id: {} from actualEntity to expectedEntity", actualEntity.getId());
        expectedEntity.setCreatedOn(actualEntity.getCreatedOn());
        log.debug("Directly copying UserRoleEntity.createdOn: {} from actualEntity to expectedEntity", actualEntity.getCreatedOn());
        expectedEntity.setActive(actualEntity.getActive());
        log.debug("Directly copying UserRoleEntity.active: {} from actualEntity to expectedEntity", actualEntity.getActive());
        // comparative copy
        if(form.getUserTypeId() != null && !form.getUserTypeId().equals(actualEntity.getUserType().getId())) {
            try {
                UserTypeVo userType = userTypeService.retrieveDetailsById(form.getUserTypeId());
                if(!userType.getActive()) {
                    log.debug("UserRoleForm.userTypeId is inactive");
                    throw new UserRoleException(AccessErrorCode.ACCESS_INACTIVE, new Object [] { "userTypeId", String.valueOf(form.getUserTypeId()) });
                }
                Optional<UserTypeEntity> optUserTypeEntity = userTypeRepository.findById(form.getUserTypeId());
                expectedEntity.setUserType(optUserTypeEntity.get());
                changeSW = true;
                log.debug("UserRoleForm.userTypeId: {} is different as UserRoleEntity.userType.id: {}",
                        form.getUserTypeId(), actualEntity.getUserType().getId());
            } catch (UserTypeException e) {
                log.debug(UserRoleMessageTemplate.MSG_TEMPLATE_USER_ROLE_DTO_USER_TYPE_ID_INVALID.getValue());
                log.error(UserRoleMessageTemplate.MSG_TEMPLATE_USER_ROLE_DTO_USER_TYPE_ID_INVALID.getValue(), e);
                throw new UserRoleException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object [] { "userTypeId", String.valueOf(form.getUserTypeId()) });
            }
        } else {
            expectedEntity.setUserType(actualEntity.getUserType());
            log.debug("UserRoleForm.userTypeId: is unchanged");
        }

        if(form.getRoleId() != null && !form.getRoleId().equals(actualEntity.getRole().getId())) {
            try {
                RoleVo role = roleService.retrieveDetailsById(form.getRoleId());
                if(!role.getActive()) {
                    log.debug("UserRoleForm.roleId is inactive");
                    throw new UserRoleException(AccessErrorCode.ACCESS_INACTIVE, new Object [] { "roleId", String.valueOf(form.getRoleId()) });
                }
                Optional<RoleEntity> optRoleEntity = roleRepository.findById(form.getRoleId());
                expectedEntity.setRole(optRoleEntity.get());
                changeSW = true;
                log.debug("UserRoleForm.roleId: {} is different as UserRoleEntity.role.id: {}",
                        form.getRoleId(), actualEntity.getRole().getId());
            } catch (RoleException e) {
                log.debug(UserRoleMessageTemplate.MSG_TEMPLATE_USER_ROLE_DTO_ROLE_ID_INVALID.getValue());
                log.error(UserRoleMessageTemplate.MSG_TEMPLATE_USER_ROLE_DTO_ROLE_ID_INVALID.getValue(), e);
                throw new UserRoleException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object [] { "roleId", String.valueOf(form.getRoleId()) });
            }
        } else {
            expectedEntity.setRole(actualEntity.getRole());
            log.debug("UserRoleForm.roleId: is unchanged");
        }
        return changeSW ? Optional.of(expectedEntity) : Optional.empty();
    }
}
