package com.teenthofabud.core.common.error;

public enum TOABErrorCode implements TOABError {

        PATCH_ATTRIBUTE_INVALID("TOAB-COMMON-001", 400),
        UNEXPECTED_CLIENT_RESPONSE_STATUS("TOAB-COMMON-002", 422),
        SYSTEM_IO_FAILURE("TOAB-COMMON-003", 422);

    private String errorCode;
    private int httpStatusCode;

    private TOABErrorCode(String errorCode, int httpStatusCode) {
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
        return "Common";
    }
}
