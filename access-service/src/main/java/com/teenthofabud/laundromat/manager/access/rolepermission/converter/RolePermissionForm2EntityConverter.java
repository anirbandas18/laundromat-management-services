package com.teenthofabud.laundromat.manager.access.rolepermission.converter;

import com.teenthofabud.core.common.handler.TOABBaseEntityAuditHandler;
import com.teenthofabud.laundromat.manager.access.permission.data.PermissionEntity;
import com.teenthofabud.laundromat.manager.access.permission.repository.PermissionRepository;
import com.teenthofabud.laundromat.manager.access.role.data.RoleEntity;
import com.teenthofabud.laundromat.manager.access.role.repository.RoleRepository;
import com.teenthofabud.laundromat.manager.access.rolepermission.data.RolePermissionEntity;
import com.teenthofabud.laundromat.manager.access.rolepermission.data.RolePermissionForm;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

import java.util.Optional;

@Component
@Slf4j
public class RolePermissionForm2EntityConverter extends TOABBaseEntityAuditHandler implements Converter<RolePermissionForm, RolePermissionEntity> {

    private PermissionRepository permissionRepository;
    private RoleRepository roleRepository;

    @Autowired
    public void setPermissionRepository(PermissionRepository permissionRepository) {
        this.permissionRepository = permissionRepository;
    }

    @Autowired
    public void setRoleRepository(RoleRepository roleRepository) {
        this.roleRepository = roleRepository;
    }

    @Override
    public RolePermissionEntity convert(RolePermissionForm form) {
        RolePermissionEntity entity = new RolePermissionEntity();
        Optional<PermissionEntity> optPermission = permissionRepository.findById(form.getPermissionId());
        entity.setPermission(optPermission.get());
        Optional<RoleEntity> optRole = roleRepository.findById(form.getRoleId());
        entity.setRole(optRole.get());
        super.assignAuditValues(entity, Boolean.TRUE);
        log.debug("Converting {} to {}", form, entity);
        return entity;
    }
}
