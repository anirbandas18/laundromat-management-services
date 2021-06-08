package com.teenthofabud.core.common.service.impl;

import com.teenthofabud.core.common.data.error.TOABBaseException;
import com.teenthofabud.core.common.data.error.TOABError;
import com.teenthofabud.core.common.data.error.TOABErrorCode;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
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
    public void validatePatches(List<PatchOperationForm> patches, String domain) throws TOABBaseException {
        for(PatchOperationForm pof : patches) {
            Errors err = new DirectFieldBindingResult(pof, pof.getClass().getSimpleName());
            patchOperationValidator.validate(pof, err);
            if(err.hasErrors()) {
                TOABError ec = TOABErrorCode.valueOf(err.getFieldError().getCode());
                throw new TOABBaseException(ec, new Object[] { err.getFieldError().getField(), domain == null ? "" : domain });
            }
        }
    }
}
