package com.teenthofabud.laundromat.manager.access.permission.validator;

import com.teenthofabud.laundromat.manager.access.error.AccessErrorCode;
import com.teenthofabud.laundromat.manager.access.operation.data.OperationException;
import com.teenthofabud.laundromat.manager.access.operation.data.OperationVo;
import com.teenthofabud.laundromat.manager.access.operation.service.OperationService;
import com.teenthofabud.laundromat.manager.access.permission.data.PermissionForm;
import com.teenthofabud.laundromat.manager.access.permission.data.PermissionMessageTemplate;
import com.teenthofabud.laundromat.manager.access.resource.data.ResourceException;
import com.teenthofabud.laundromat.manager.access.resource.data.ResourceVo;
import com.teenthofabud.laundromat.manager.access.resource.service.ResourceService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

@Component
@Slf4j
public class PermissionFormValidator implements Validator {

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
        return clazz.isAssignableFrom(PermissionForm.class);
    }

    @Override
    public void validate(Object target, Errors errors) {
        PermissionForm form = (PermissionForm) target;

        if(form.getResourceId() == null || form.getResourceId() <= 0L) {
            log.debug("PermissionForm.resourceId is invalid");
            errors.rejectValue("resourceId", AccessErrorCode.ACCESS_ATTRIBUTE_INVALID.name());
            return;
        } else {
            try {
                ResourceVo resource = resourceService.retrieveDetailsById(form.getResourceId());
                if(!resource.getActive()) {
                    log.debug(PermissionMessageTemplate.MSG_TEMPLATE_PERMISSION_DTO_RESOURCE_ID_INACTIVE.getValue());
                    errors.rejectValue("resourceId", AccessErrorCode.ACCESS_INACTIVE.name());
                    return;
                }
            } catch (ResourceException e) {
                log.debug(PermissionMessageTemplate.MSG_TEMPLATE_PERMISSION_DTO_RESOURCE_ID_INVALID.getValue());
                log.error(PermissionMessageTemplate.MSG_TEMPLATE_PERMISSION_DTO_RESOURCE_ID_INVALID.getValue(), e);
                errors.rejectValue("resourceId", AccessErrorCode.ACCESS_ATTRIBUTE_INVALID.name());
                return;
            }
        }

        if(form.getOperationId() == null || form.getOperationId() <= 0L) {
            log.debug("PermissionForm.operationId is invalid");
            errors.rejectValue("operationId", AccessErrorCode.ACCESS_ATTRIBUTE_INVALID.name());
            return;
        } else {
            try {
                OperationVo operation = operationService.retrieveDetailsById(form.getOperationId());
                if(!operation.getActive()) {
                    log.debug(PermissionMessageTemplate.MSG_TEMPLATE_PERMISSION_DTO_OPERATION_ID_INACTIVE.getValue());
                    errors.rejectValue("operationId", AccessErrorCode.ACCESS_INACTIVE.name());
                    return;
                }
            } catch (OperationException e) {
                log.debug(PermissionMessageTemplate.MSG_TEMPLATE_PERMISSION_DTO_OPERATION_ID_INVALID.getValue());
                log.error(PermissionMessageTemplate.MSG_TEMPLATE_PERMISSION_DTO_OPERATION_ID_INVALID.getValue(), e);
                errors.rejectValue("operationId", AccessErrorCode.ACCESS_ATTRIBUTE_INVALID.name());
                return;
            }
        }
    }
}
