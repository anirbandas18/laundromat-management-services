package com.teenthofabud.laundromat.manager.error;

import com.teenthofabud.core.common.model.error.TOABError;

public enum TypeErrorCode implements TOABError {

    TYPE_ATTRIBUTE_INVALID("LMS-TYPE-001", 400), // syntactic
    TYPE_NOT_FOUND("LMS-TYPE-002", 404),
    TYPE_ATTRIBUTE_UNEXPECTED("LMS-TYPE-003", 422), // semantic
    TYPE_EXISTS("LMS-TYPE-004", 409),
    TYPE_INACTIVE("LMS-TYPE-005", 400),
    TYPE_ACTION_FAILURE("LMS-TYPE-006", 500);

    private String errorCode;
    private int httpStatusCode;

    private TypeErrorCode(String errorCode, int httpStatusCode) {
        this.errorCode = errorCode;
        this.httpStatusCode = httpStatusCode;
    }

    @Override
    public String toString() {
        return "ErrorCode{" +
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
        return "Type";
    }
}
