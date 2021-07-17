package com.teenthofabud.laundromat.manager.access.error;

import com.teenthofabud.core.common.error.TOABError;

public enum AccessErrorCode implements TOABError {

    ACCESS_ATTRIBUTE_INVALID("LMS-ACCESS-001", 400), // syntactic
    ACCESS_NOT_FOUND("LMS-ACCESS-002", 404),
    ACCESS_ATTRIBUTE_UNEXPECTED("LMS-ACCESS-003", 422), // semantic
    ACCESS_EXISTS("LMS-ACCESS-004", 409),
    ACCESS_INACTIVE("LMS-ACCESS-005", 400),
    ACCESS_ACTION_FAILURE("LMS-ACCESS-006", 500);

    private String errorCode;
    private int httpStatusCode;

    private AccessErrorCode(String errorCode, int httpStatusCode) {
        this.errorCode = errorCode;
        this.httpStatusCode = httpStatusCode;
    }

    @Override
    public String toString() {
        return "AccessErrorCode{" +
                this.name() + " -> " +
                "errorCode='" + errorCode + '\'' +
                ", httpStatusCode=" + httpStatusCode +
                '}';
    }

    @Override
    public String getErrorCode() {
        return this.errorCode;
    }

    @Override
    public Integer getHttpStatusCode() {
        return this.httpStatusCode;
    }

    @Override
    public String getDomain() {
        return "Access";
    }

}
