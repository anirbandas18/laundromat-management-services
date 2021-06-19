package com.teenthofabud.core.common.service.impl;

import com.teenthofabud.core.common.error.TOABError;
import com.teenthofabud.core.common.error.TOABErrorCode;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.error.TOABSystemException;
import com.teenthofabud.core.common.service.TOABBaseService;
import com.teenthofabud.core.common.validator.PatchOperationFormValidator;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.validation.DirectFieldBindingResult;
import org.springframework.validation.Errors;

import java.util.List;

@Slf4j
public class TOABBaseServiceImpl implements TOABBaseService {

    private PatchOperationFormValidator patchOperationValidator;

    @Autowired
    public void setPatchOperationValidator(PatchOperationFormValidator patchOperationValidator) {
        this.patchOperationValidator = patchOperationValidator;
    }

    @Override
    public void validatePatches(List<PatchOperationForm> patches, String domain) throws TOABSystemException {
        for(PatchOperationForm pof : patches) {
            Errors err = new DirectFieldBindingResult(pof, pof.getClass().getSimpleName());
            patchOperationValidator.validate(pof, err);
            if(err.hasErrors()) {
                TOABError ec = TOABErrorCode.valueOf(err.getFieldError().getCode());
                throw new TOABSystemException(ec, new Object[] { err.getFieldError().getField() });
            }
        }
    }
}
