package com.teenthofabud.laundromat.manager.access.permission.converter;

import com.teenthofabud.core.common.converter.ComparativePatchConverter;
import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.laundromat.manager.access.error.AccessErrorCode;
import com.teenthofabud.laundromat.manager.access.operation.data.OperationEntity;
import com.teenthofabud.laundromat.manager.access.operation.data.OperationException;
import com.teenthofabud.laundromat.manager.access.operation.data.OperationVo;
import com.teenthofabud.laundromat.manager.access.operation.repository.OperationRepository;
import com.teenthofabud.laundromat.manager.access.operation.service.OperationService;
import com.teenthofabud.laundromat.manager.access.permission.data.PermissionDto;
import com.teenthofabud.laundromat.manager.access.permission.data.PermissionEntity;
import com.teenthofabud.laundromat.manager.access.permission.data.PermissionException;
import com.teenthofabud.laundromat.manager.access.permission.data.PermissionMessageTemplate;
import com.teenthofabud.laundromat.manager.access.resource.data.ResourceEntity;
import com.teenthofabud.laundromat.manager.access.resource.data.ResourceException;
import com.teenthofabud.laundromat.manager.access.resource.data.ResourceVo;
import com.teenthofabud.laundromat.manager.access.resource.repository.ResourceRepository;
import com.teenthofabud.laundromat.manager.access.resource.service.ResourceService;
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
public class PermissionDto2EntityConverter implements ComparativePatchConverter<PermissionDto, PermissionEntity> {

    private static final Integer NO_OF_COMPARABLE_AND_MAPPABLE_FIELDS = 3;

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
    public void compareAndMap(PermissionDto dto, PermissionEntity actualEntity) throws TOABBaseException {
        boolean[] changeSW = new boolean[NO_OF_COMPARABLE_AND_MAPPABLE_FIELDS]; // size = number of attributes in dto
        Arrays.fill(changeSW, Boolean.FALSE);
        int i = 0;

        Optional<String> optResourceId = dto.getResourceId();
        if(optResourceId.isPresent()) {
            Long resourceId = Long.parseLong(optResourceId.get());
            try {
                ResourceVo resource = resourceService.retrieveDetailsById(resourceId);
                if(!resource.getActive()) {
                    log.debug(PermissionMessageTemplate.MSG_TEMPLATE_PERMISSION_DTO_RESOURCE_ID_INACTIVE.getValue());
                    throw new PermissionException(AccessErrorCode.ACCESS_INACTIVE, new Object[] { "resourceId" });
                }
            } catch (ResourceException e) {
                log.debug(PermissionMessageTemplate.MSG_TEMPLATE_PERMISSION_DTO_RESOURCE_ID_INVALID.getValue());
                log.error(PermissionMessageTemplate.MSG_TEMPLATE_PERMISSION_DTO_RESOURCE_ID_INVALID.getValue(), e);
                throw new PermissionException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[] { "resourceId" });
            }
            Optional<ResourceEntity> optResource = resourceRepository.findById(resourceId);
            actualEntity.setResource(optResource.get());
            changeSW[i++] = true;
            log.debug("PermissionDto.resourceId is valid");
        }

        Optional<String> optOperationId = dto.getOperationId();
        if(optOperationId.isPresent()) {
            Long operationId = Long.parseLong(optOperationId.get());
            try {
                OperationVo operation = operationService.retrieveDetailsById(operationId);
                if(!operation.getActive()) {
                    log.debug(PermissionMessageTemplate.MSG_TEMPLATE_PERMISSION_DTO_OPERATION_ID_INACTIVE.getValue());
                    throw new PermissionException(AccessErrorCode.ACCESS_INACTIVE, new Object[] { "operationId" });
                }
            } catch (OperationException e) {
                log.debug(PermissionMessageTemplate.MSG_TEMPLATE_PERMISSION_DTO_OPERATION_ID_INVALID.getValue());
                log.error(PermissionMessageTemplate.MSG_TEMPLATE_PERMISSION_DTO_OPERATION_ID_INVALID.getValue(), e);
                throw new PermissionException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[] { "operationId" });
            }
            Optional<OperationEntity> optOperation = operationRepository.findById(operationId);
            actualEntity.setOperation(optOperation.get());
            changeSW[i++] = true;
            log.debug("PermissionDto.operationId is valid");
        }

        Optional<String> optActive = dto.getActive();
        if(optActive.isPresent()) {
            actualEntity.setActive(Boolean.valueOf(optActive.get()));
            changeSW[i++] = true;
            log.debug("OperationDto.active is valid");
        }
        if(Collections.frequency(Arrays.asList(changeSW), Boolean.TRUE) >= 1) {
            log.debug("All provided OperationDto attributes are valid");
            actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));
            return;
        }
        log.debug("Not all provided OperationDto attributes are valid");
    }
}
