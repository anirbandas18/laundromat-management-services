package com.teenthofabud.laundromat.manager.access.permission.mapper;

import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.core.common.mapper.DualChannelMapper;
import com.teenthofabud.laundromat.manager.access.error.AccessErrorCode;
import com.teenthofabud.laundromat.manager.access.operation.data.OperationEntity;
import com.teenthofabud.laundromat.manager.access.operation.data.OperationException;
import com.teenthofabud.laundromat.manager.access.operation.data.OperationVo;
import com.teenthofabud.laundromat.manager.access.operation.repository.OperationRepository;
import com.teenthofabud.laundromat.manager.access.operation.service.OperationService;
import com.teenthofabud.laundromat.manager.access.permission.data.PermissionEntity;
import com.teenthofabud.laundromat.manager.access.permission.data.PermissionException;
import com.teenthofabud.laundromat.manager.access.permission.data.PermissionForm;
import com.teenthofabud.laundromat.manager.access.permission.data.PermissionMessageTemplate;
import com.teenthofabud.laundromat.manager.access.resource.data.ResourceEntity;
import com.teenthofabud.laundromat.manager.access.resource.data.ResourceException;
import com.teenthofabud.laundromat.manager.access.resource.data.ResourceVo;
import com.teenthofabud.laundromat.manager.access.resource.repository.ResourceRepository;
import com.teenthofabud.laundromat.manager.access.resource.service.ResourceService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.Optional;

@Component
@Slf4j
public class PermissionForm2EntityMapper implements DualChannelMapper<PermissionEntity, PermissionForm> {

    private ResourceService resourceService;
    private OperationService operationService;
    private ResourceRepository resourceRepository;
    private OperationRepository operationRepository;

    @Autowired
    public void setResourceService(ResourceService resourceService) {
        this.resourceService = resourceService;
    }

    @Autowired
    public void setOperationService(OperationService operationService) {
        this.operationService = operationService;
    }

    @Autowired
    public void setResourceRepository(ResourceRepository resourceRepository) {
        this.resourceRepository = resourceRepository;
    }

    @Autowired
    public void setOperationRepository(OperationRepository operationRepository) {
        this.operationRepository = operationRepository;
    }

    @Override
    public Optional<PermissionEntity> compareAndMap(PermissionEntity actualEntity, PermissionForm form) throws TOABBaseException {
        PermissionEntity expectedEntity = new PermissionEntity();
        boolean changeSW = false;
        // direct copy
        expectedEntity.setId(actualEntity.getId());
        log.debug("Directly copying PermissionEntity.id: {} from actualEntity to expectedEntity", actualEntity.getId());
        expectedEntity.setCreatedOn(actualEntity.getCreatedOn());
        log.debug("Directly copying PermissionEntity.createdOn: {} from actualEntity to expectedEntity", actualEntity.getCreatedOn());
        expectedEntity.setActive(actualEntity.getActive());
        log.debug("Directly copying PermissionEntity.active: {} from actualEntity to expectedEntity", actualEntity.getActive());
        // comparative copy
        if(form.getResourceId() != null && !form.getResourceId().equals(actualEntity.getResource().getId())) {
            try {
                ResourceVo resource = resourceService.retrieveDetailsById(form.getResourceId());
                if(!resource.getActive()) {
                    log.debug("PermissionForm.resourceId is inactive");
                    throw new PermissionException(AccessErrorCode.ACCESS_INACTIVE, new Object [] { "resourceId", String.valueOf(form.getResourceId()) });
                }
                Optional<ResourceEntity> optResourceEntity = resourceRepository.findById(form.getResourceId());
                expectedEntity.setResource(optResourceEntity.get());
                changeSW = true;
                log.debug("PermissionForm.resourceId: {} is different as PermissionEntity.resource.id: {}",
                        form.getResourceId(), actualEntity.getResource().getId());
            } catch (ResourceException e) {
                log.debug(PermissionMessageTemplate.MSG_TEMPLATE_PERMISSION_DTO_RESOURCE_ID_INVALID.getValue());
                log.error(PermissionMessageTemplate.MSG_TEMPLATE_PERMISSION_DTO_RESOURCE_ID_INVALID.getValue(), e);
                throw new PermissionException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object [] { "resourceId", String.valueOf(form.getResourceId()) });
            }
        } else {
            expectedEntity.setResource(actualEntity.getResource());
            log.debug("PermissionForm.resourceId: is unchanged");
        }

        if(form.getOperationId() != null && !form.getOperationId().equals(actualEntity.getOperation().getId())) {
            try {
                OperationVo operation = operationService.retrieveDetailsById(form.getOperationId());
                if(!operation.getActive()) {
                    log.debug("PermissionForm.operationId is inactive");
                    throw new PermissionException(AccessErrorCode.ACCESS_INACTIVE, new Object [] { "operationId", String.valueOf(form.getOperationId()) });
                }
                Optional<OperationEntity> optOperationEntity = operationRepository.findById(form.getOperationId());
                expectedEntity.setOperation(optOperationEntity.get());
                changeSW = true;
                log.debug("PermissionForm.operationId: {} is different as PermissionEntity.operation.id: {}",
                        form.getOperationId(), actualEntity.getOperation().getId());
            } catch (OperationException e) {
                log.debug(PermissionMessageTemplate.MSG_TEMPLATE_PERMISSION_DTO_OPERATION_ID_INVALID.getValue());
                log.error(PermissionMessageTemplate.MSG_TEMPLATE_PERMISSION_DTO_OPERATION_ID_INVALID.getValue(), e);
                throw new PermissionException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object [] { "operationId", String.valueOf(form.getOperationId()) });
            }
        } else {
            expectedEntity.setOperation(actualEntity.getOperation());
            log.debug("PermissionForm.operationId: is unchanged");
        }
        return changeSW ? Optional.of(expectedEntity) : Optional.empty();
    }
}
