package com.teenthofabud.core.common.data.error;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class TOABFeignException extends Exception {

    private transient String errorCode;
    private transient String errorMessage;
    private transient String errorDomain;

    public TOABFeignException(String errorCode, String errorMessage) {
        super(errorMessage);
        this.errorCode = errorCode;
        this.errorMessage = errorMessage;
    }

    public TOABFeignException(String errorCode, String errorMessage, String errorDomain) {
        super(errorMessage);
        this.errorCode = errorCode;
        this.errorMessage = errorMessage;
        this.errorDomain = errorDomain;
    }

}
