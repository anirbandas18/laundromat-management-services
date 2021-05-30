package com.teenthofabud.core.common.validator;

import com.teenthofabud.core.common.model.error.TOABErrorCode;
import com.teenthofabud.core.common.model.form.PatchOperationForm;
import org.springframework.util.StringUtils;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;

import javax.json.JsonPatch;

public class PatchOperationFormValidator implements Validator {

    @Override
    public boolean supports(Class<?> clazz) {
        return clazz.isAssignableFrom(PatchOperationForm.class);
    }

    @Override
    public void validate(Object target, Errors errors) {
        PatchOperationForm form = (PatchOperationForm) target;
        if(StringUtils.isEmpty(form)) {
            errors.rejectValue("op", TOABErrorCode.PATCH_ATTRIBUTE_INVALID.name());
            return;
        } else {
            try {
                JsonPatch.Operation.valueOf(form.getOp().toUpperCase());
            } catch (IllegalArgumentException e) {
                errors.rejectValue("op", TOABErrorCode.PATCH_ATTRIBUTE_INVALID.name());
                return;
            }
        }
        if(StringUtils.isEmpty(form.getPath())) {
            errors.rejectValue("path", TOABErrorCode.PATCH_ATTRIBUTE_INVALID.name());
            return;
        } else if (!form.getPath().matches("\\/(\\S+)*(\\/\\S*)*")) {
            errors.rejectValue("path", TOABErrorCode.PATCH_ATTRIBUTE_INVALID.name());
            return;
        }
        if(StringUtils.isEmpty(form.getValue())) {
            errors.rejectValue("value", TOABErrorCode.PATCH_ATTRIBUTE_INVALID.name());
            return;
        }
    }
}
