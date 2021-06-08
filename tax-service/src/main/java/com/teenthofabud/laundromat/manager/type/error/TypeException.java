package com.teenthofabud.laundromat.manager.type.error;

import com.teenthofabud.core.common.data.error.TOABBaseException;
import com.teenthofabud.core.common.data.error.TOABError;
import com.teenthofabud.core.common.data.error.TOABFeignException;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
public class TypeException extends TOABFeignException {

    public TypeException(String errorCode, String errorMessage) {
        super(errorCode, errorMessage);
    }

    public TypeException(String errorCode, String errorMessage, String errorDomain) {
        super(errorCode, errorMessage, errorDomain);
    }
}
