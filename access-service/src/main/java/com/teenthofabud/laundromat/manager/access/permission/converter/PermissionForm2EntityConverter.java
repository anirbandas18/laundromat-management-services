package com.teenthofabud.laundromat.manager.access.permission.converter;

import com.teenthofabud.core.common.handler.TOABBaseEntityAuditHandler;
import com.teenthofabud.laundromat.manager.access.operation.data.OperationEntity;
import com.teenthofabud.laundromat.manager.access.operation.repository.OperationRepository;
import com.teenthofabud.laundromat.manager.access.permission.data.PermissionEntity;
import com.teenthofabud.laundromat.manager.access.permission.data.PermissionForm;
import com.teenthofabud.laundromat.manager.access.resource.data.ResourceEntity;
import com.teenthofabud.laundromat.manager.access.resource.repository.ResourceRepository;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

import java.util.Optional;

@Component
@Slf4j
public class PermissionForm2EntityConverter extends TOABBaseEntityAuditHandler implements Converter<PermissionForm, PermissionEntity> {

    private ResourceRepository resourceRepository;
    private OperationRepository operationRepository;

    @Autowired
    public void setResourceRepository(ResourceRepository resourceRepository) {
        this.resourceRepository = resourceRepository;
    }

    @Autowired
    public void setOperationRepository(OperationRepository operationRepository) {
        this.operationRepository = operationRepository;
    }

    @Override
    public PermissionEntity convert(PermissionForm form) {
        PermissionEntity entity = new PermissionEntity();
        Optional<ResourceEntity> optResource = resourceRepository.findById(form.getResourceId());
        entity.setResource(optResource.get());
        Optional<OperationEntity> optOperation = operationRepository.findById(form.getOperationId());
        entity.setOperation(optOperation.get());
        super.assignAuditValues(entity, Boolean.TRUE);
        log.debug("Converting {} to {}", form, entity);
        return entity;
    }
}
