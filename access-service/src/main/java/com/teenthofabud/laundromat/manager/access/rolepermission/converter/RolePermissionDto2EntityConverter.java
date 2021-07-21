package com.teenthofabud.laundromat.manager.access.rolepermission.converter;

import com.teenthofabud.core.common.converter.ComparativePatchConverter;
import com.teenthofabud.core.common.error.TOABBaseException;
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
import com.teenthofabud.laundromat.manager.access.rolepermission.data.RolePermissionDto;
import com.teenthofabud.laundromat.manager.access.rolepermission.data.RolePermissionEntity;
import com.teenthofabud.laundromat.manager.access.rolepermission.data.RolePermissionException;
import com.teenthofabud.laundromat.manager.access.rolepermission.data.RolePermissionMessageTemplate;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.Arrays;
import java.util.Collections;
import java.util.Optional;

@Component
@Slf4j
public class RolePermissionDto2EntityConverter implements ComparativePatchConverter<RolePermissionDto, RolePermissionEntity> {

    private static final Integer NO_OF_COMPARABLE_AND_MAPPABLE_FIELDS = 3;

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
    public void compareAndMap(RolePermissionDto dto, RolePermissionEntity actualEntity) throws TOABBaseException {
        boolean[] changeSW = new boolean[NO_OF_COMPARABLE_AND_MAPPABLE_FIELDS]; // size = number of attributes in dto
        Arrays.fill(changeSW, Boolean.FALSE);
        int i = 0;

        Optional<String> optPermissionId = dto.getPermissionId();
        if(optPermissionId.isPresent()) {
            Long permissionId = Long.parseLong(optPermissionId.get());
            try {
                PermissionVo permission = permissionService.retrieveDetailsById(permissionId);
                if(!permission.getActive()) {
                    log.debug(RolePermissionMessageTemplate.MSG_TEMPLATE_ROLE_PERMISSION_DTO_PERMISSION_ID_INACTIVE.getValue());
                    throw new RolePermissionException(AccessErrorCode.ACCESS_INACTIVE, new Object[] { "permissionId" });
                }
            } catch (PermissionException e) {
                log.debug(RolePermissionMessageTemplate.MSG_TEMPLATE_ROLE_PERMISSION_DTO_PERMISSION_ID_INVALID.getValue());
                log.error(RolePermissionMessageTemplate.MSG_TEMPLATE_ROLE_PERMISSION_DTO_PERMISSION_ID_INVALID.getValue(), e);
                throw new RolePermissionException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[] { "permissionId" });
            }
            Optional<PermissionEntity> optPermission = permissionRepository.findById(permissionId);
            actualEntity.setPermission(optPermission.get());
            changeSW[i++] = true;
            log.debug("RolePermissionDto.permissionId is valid");
        }

        Optional<String> optRoleId = dto.getRoleId();
        if(optRoleId.isPresent()) {
            Long roleId = Long.parseLong(optRoleId.get());
            try {
                RoleVo role = roleService.retrieveDetailsById(roleId);
                if(!role.getActive()) {
                    log.debug(RolePermissionMessageTemplate.MSG_TEMPLATE_ROLE_PERMISSION_DTO_ROLE_ID_INACTIVE.getValue());
                    throw new RolePermissionException(AccessErrorCode.ACCESS_INACTIVE, new Object[] { "roleId" });
                }
            } catch (RoleException e) {
                log.debug(RolePermissionMessageTemplate.MSG_TEMPLATE_ROLE_PERMISSION_DTO_ROLE_ID_INVALID.getValue());
                log.error(RolePermissionMessageTemplate.MSG_TEMPLATE_ROLE_PERMISSION_DTO_ROLE_ID_INVALID.getValue(), e);
                throw new RolePermissionException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[] { "roleId" });
            }
            Optional<RoleEntity> optRole = roleRepository.findById(roleId);
            actualEntity.setRole(optRole.get());
            changeSW[i++] = true;
            log.debug("RolePermissionDto.roleId is valid");
        }

        Optional<String> optActive = dto.getActive();
        if(optActive.isPresent()) {
            actualEntity.setActive(Boolean.valueOf(optActive.get()));
            changeSW[i++] = true;
            log.debug("RolePermissionDto.active is valid");
        }
        if(Collections.frequency(Arrays.asList(changeSW), Boolean.TRUE) >= 1) {
            log.debug("All provided RolePermissionDto attributes are valid");
            actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));
            return;
        }
        log.debug("Not all provided RolePermissionDto attributes are valid");
    }
}
