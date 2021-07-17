package com.teenthofabud.laundromat.manager.access.permission.validator;

import com.teenthofabud.laundromat.manager.access.error.AccessErrorCode;
import com.teenthofabud.laundromat.manager.access.operation.data.OperationException;
import com.teenthofabud.laundromat.manager.access.operation.data.OperationVo;
import com.teenthofabud.laundromat.manager.access.operation.service.OperationService;
import com.teenthofabud.laundromat.manager.access.permission.data.PermissionDto;
import com.teenthofabud.laundromat.manager.access.permission.data.PermissionMessageTemplate;
import com.teenthofabud.laundromat.manager.access.resource.data.ResourceException;
import com.teenthofabud.laundromat.manager.access.resource.data.ResourceVo;
import com.teenthofabud.laundromat.manager.access.resource.service.ResourceService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import java.util.Optional;

@Component
@Slf4j
public class PermissionDtoValidator implements Validator {

    private ResourceService resourceService;
    private OperationService operationService;

    @Autowired
    public void setResourceService(ResourceService resourceService) {
        this.resourceService = resourceService;
    }

    @Autowired
    public void setOperationService(OperationService operationService) {
        this.operationService = operationService;
    }

    @Override
    public boolean supports(Class<?> clazz) {
        return clazz.isAssignableFrom(PermissionDto.class);
    }

    @Override
    public void validate(Object target, Errors errors) {
        PermissionDto dto = (PermissionDto) target;

        Optional<String> optResourceId = dto.getResourceId();
        if(optResourceId.isPresent() && StringUtils.hasText(StringUtils.trimWhitespace(optResourceId.get()))) {
            boolean isValid = true;
            try {
                Long resourceId = Long.parseLong(optResourceId.get());
                if(resourceId <= 0L) {
                    isValid = false;
                    log.debug("PermissionDto.resourceId is invalid: resourceId <= 0");
                } else {
                    ResourceVo resource = resourceService.retrieveDetailsById(resourceId);
                    if(!resource.getActive()) {
                        isValid = false;
                        log.debug(PermissionMessageTemplate.MSG_TEMPLATE_PERMISSION_DTO_RESOURCE_ID_INACTIVE.getValue());
                    }
                }
            } catch (NumberFormatException | ResourceException e) {
                isValid = false;
                log.debug(PermissionMessageTemplate.MSG_TEMPLATE_PERMISSION_DTO_RESOURCE_ID_INVALID.getValue());
                log.error(PermissionMessageTemplate.MSG_TEMPLATE_PERMISSION_DTO_RESOURCE_ID_INVALID.getValue(), e);
            }
            if(!isValid) {
                errors.rejectValue("resourceId", AccessErrorCode.ACCESS_ATTRIBUTE_INVALID.name());
                return;
            }
        }


        Optional<String> optOperationId = dto.getOperationId();
        if(optOperationId.isPresent() && StringUtils.hasText(StringUtils.trimWhitespace(optOperationId.get()))) {
            boolean isValid = true;
            try {
                Long operationId = Long.parseLong(optOperationId.get());
                if(operationId <= 0L) {
                    isValid = false;
                    log.debug("PermissionDto.operationId is invalid: operationId <= 0");
                } else {
                    OperationVo operation = operationService.retrieveDetailsById(operationId);
                    if(!operation.getActive()) {
                        isValid = false;
                        log.debug(PermissionMessageTemplate.MSG_TEMPLATE_PERMISSION_DTO_OPERATION_ID_INACTIVE.getValue());
                    }
                }
            } catch (NumberFormatException | OperationException e) {
                isValid = false;
                log.debug(PermissionMessageTemplate.MSG_TEMPLATE_PERMISSION_DTO_OPERATION_ID_INVALID.getValue());
                log.error(PermissionMessageTemplate.MSG_TEMPLATE_PERMISSION_DTO_OPERATION_ID_INVALID.getValue(), e);
            }
            if(!isValid) {
                errors.rejectValue("operationId", AccessErrorCode.ACCESS_ATTRIBUTE_INVALID.name());
                return;
            }
        }

        Optional<String> optActive = dto.getActive();
        if(optActive.isPresent() && StringUtils.hasText(StringUtils.trimWhitespace(optActive.get()))) {
            Boolean trueSW = optActive.get().equalsIgnoreCase(Boolean.TRUE.toString());
            Boolean falseSW = optActive.get().equalsIgnoreCase(Boolean.FALSE.toString());
            if(!trueSW && !falseSW) {
                errors.rejectValue("active", AccessErrorCode.ACCESS_ATTRIBUTE_INVALID.name());
                log.debug("PermissionDto.active is invalid");
                return;
            }
        }
    }
}
