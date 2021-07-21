package com.teenthofabud.laundromat.manager.access.rolepermission.mapper;

import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.core.common.mapper.DualChannelMapper;
import com.teenthofabud.laundromat.manager.access.error.AccessErrorCode;
import com.teenthofabud.laundromat.manager.access.permission.data.PermissionEntity;
import com.teenthofabud.laundromat.manager.access.permission.data.PermissionException;
import com.teenthofabud.laundromat.manager.access.permission.data.PermissionVo;
import com.teenthofabud.laundromat.manager.access.permission.repository.PermissionRepository;
import com.teenthofabud.laundromat.manager.access.permission.service.PermissionService;
import com.teenthofabud.laundromat.manager.access.role.data.RoleEntity;
import com.teenthofabud.laundromat.manager.access.role.data.RoleException;
import com.teenthofabud.laundromat.manager.access.role.data.RoleVo;
import com.teenthofabud.laundromat.manager.access.role.repository.RoleRepository;
import com.teenthofabud.laundromat.manager.access.role.service.RoleService;
import com.teenthofabud.laundromat.manager.access.rolepermission.data.RolePermissionEntity;
import com.teenthofabud.laundromat.manager.access.rolepermission.data.RolePermissionException;
import com.teenthofabud.laundromat.manager.access.rolepermission.data.RolePermissionForm;
import com.teenthofabud.laundromat.manager.access.rolepermission.data.RolePermissionMessageTemplate;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.Optional;

@Component
@Slf4j
public class RolePermissionForm2EntityMapper implements DualChannelMapper<RolePermissionEntity, RolePermissionForm> {

    private PermissionService permissionService;
    private RoleService roleService;
    private PermissionRepository permissionRepository;
    private RoleRepository roleRepository;

    @Autowired
    public void setPermissionService(PermissionService permissionService) {
        this.permissionService = permissionService;
    }

    @Autowired
    public void setRoleService(RoleService roleService) {
        this.roleService = roleService;
    }

    @Autowired
    public void setPermissionRepository(PermissionRepository permissionRepository) {
        this.permissionRepository = permissionRepository;
    }

    @Autowired
    public void setRoleRepository(RoleRepository roleRepository) {
        this.roleRepository = roleRepository;
    }

    @Override
    public Optional<RolePermissionEntity> compareAndMap(RolePermissionEntity actualEntity, RolePermissionForm form) throws TOABBaseException {
        RolePermissionEntity expectedEntity = new RolePermissionEntity();
        boolean changeSW = false;
        // direct copy
        expectedEntity.setId(actualEntity.getId());
        log.debug("Directly copying RolePermissionEntity.id: {} from actualEntity to expectedEntity", actualEntity.getId());
        expectedEntity.setCreatedOn(actualEntity.getCreatedOn());
        log.debug("Directly copying RolePermissionEntity.createdOn: {} from actualEntity to expectedEntity", actualEntity.getCreatedOn());
        expectedEntity.setActive(actualEntity.getActive());
        log.debug("Directly copying RolePermissionEntity.active: {} from actualEntity to expectedEntity", actualEntity.getActive());
        // comparative copy
        if(form.getPermissionId() != null && !form.getPermissionId().equals(actualEntity.getPermission().getId())) {
            try {
                PermissionVo permission = permissionService.retrieveDetailsById(form.getPermissionId());
                if(!permission.getActive()) {
                    log.debug("RolePermissionForm.permissionId is inactive");
                    throw new RolePermissionException(AccessErrorCode.ACCESS_INACTIVE, new Object [] { "permissionId", String.valueOf(form.getPermissionId()) });
                }
                Optional<PermissionEntity> optPermissionEntity = permissionRepository.findById(form.getPermissionId());
                expectedEntity.setPermission(optPermissionEntity.get());
                changeSW = true;
                log.debug("RolePermissionForm.permissionId: {} is different as RolePermissionEntity.permission.id: {}",
                        form.getPermissionId(), actualEntity.getPermission().getId());
            } catch (PermissionException e) {
                log.debug(RolePermissionMessageTemplate.MSG_TEMPLATE_ROLE_PERMISSION_DTO_PERMISSION_ID_INVALID.getValue());
                log.error(RolePermissionMessageTemplate.MSG_TEMPLATE_ROLE_PERMISSION_DTO_PERMISSION_ID_INVALID.getValue(), e);
                throw new RolePermissionException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object [] { "permissionId", String.valueOf(form.getPermissionId()) });
            }
        } else {
            expectedEntity.setPermission(actualEntity.getPermission());
            log.debug("RolePermissionForm.permissionId: is unchanged");
        }

        if(form.getRoleId() != null && !form.getRoleId().equals(actualEntity.getRole().getId())) {
            try {
                RoleVo role = roleService.retrieveDetailsById(form.getRoleId());
                if(!role.getActive()) {
                    log.debug("RolePermissionForm.roleId is inactive");
                    throw new RolePermissionException(AccessErrorCode.ACCESS_INACTIVE, new Object [] { "roleId", String.valueOf(form.getRoleId()) });
                }
                Optional<RoleEntity> optRoleEntity = roleRepository.findById(form.getRoleId());
                expectedEntity.setRole(optRoleEntity.get());
                changeSW = true;
                log.debug("RolePermissionForm.roleId: {} is different as RolePermissionEntity.role.id: {}",
                        form.getRoleId(), actualEntity.getRole().getId());
            } catch (RoleException e) {
                log.debug(RolePermissionMessageTemplate.MSG_TEMPLATE_ROLE_PERMISSION_DTO_ROLE_ID_INVALID.getValue());
                log.error(RolePermissionMessageTemplate.MSG_TEMPLATE_ROLE_PERMISSION_DTO_ROLE_ID_INVALID.getValue(), e);
                throw new RolePermissionException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object [] { "roleId", String.valueOf(form.getRoleId()) });
            }
        } else {
            expectedEntity.setRole(actualEntity.getRole());
            log.debug("RolePermissionForm.roleId: is unchanged");
        }
        return changeSW ? Optional.of(expectedEntity) : Optional.empty();
    }
}
