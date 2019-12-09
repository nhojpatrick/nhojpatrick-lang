/*
 * Copyright (c) 2015 https://github.com/nhojpatrick
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated
 * documentation files (the "Software"), to deal in the Software without restriction, including without limitation the
 * rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the
 * Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
 * WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
 * OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */
package com.github.nhojpatrick.lang;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * The <code>Trinary</code> enum is a strongly typed alternative to; true, false
 * and null.
 *
 * @author nhojpatrick
 * @nhojpatrick.todo [IMPL] Trinary String's are lowercase, but valueOf(String) only works
 *       with uppercase by default, i.e. the enum variable name. Determine how
 *       this should be resolved.
 */
public enum Trinary {

    /**
     * Trinary FALSE.
     */
    FALSE('F'),

    /**
     * Trinary TRUE.
     */
    TRUE('T'),

    /**
     * Trinary UNKNOWN.
     */
    UNKNOWN('U');

    private static final Logger LOGGER = LoggerFactory.getLogger(Trinary.class);

    /**
     * Returns the Trinary enum constant that represents the supplied pBoolean.
     * Supplying null will return UNKNOWN, Boolean.TRUE will return TRUE and
     * Boolean.FALSE will return FALSE.
     *
     * @param pBoolean
     *            the Boolean that needs to be resolved to its Trinary enum
     *            representation.
     * @return the Trinary that represents the supplied pBoolean.
     */
    public static Trinary valueOf(final Boolean pBoolean) {

        LOGGER.debug("ENTRY[valueOf=Boolean;Boolean=" + pBoolean + "]");

        final Trinary trinary;
        if (pBoolean == null) {

            trinary = UNKNOWN;

        } else if (pBoolean) {
            trinary = TRUE;

        } else {
            trinary = FALSE;
        }

        LOGGER.debug("EXIT[valueOf=Boolean;Trinary=" + trinary + "]");

        return trinary;
    }

    /**
     * Returns the Trinary enum constant that represents the supplied pChar. The
     * value 'U' will return UNKNOWN, 'T' will return TRUE and 'F' will return
     * FALSE;
     *
     * @param pChar
     *            the char that needs to be resolved to its Trinary enum
     *            representation.
     * @return the Trinary that represents the supplied pChar.
     */
    public static Trinary valueOf(final char pChar) {

        LOGGER.debug("ENTRY[valueOf=char;char=" + pChar + "]");

        final Trinary trinary;
        if ('U' == pChar) {
            trinary = UNKNOWN;

        } else if ('T' == pChar) {
            trinary = TRUE;

        } else if ('F' == pChar) {
            trinary = FALSE;

        } else {
            final IllegalArgumentException e = new IllegalArgumentException(
                    "TODO [NHOJ-LANG-000001] No Trinary enum constant for '" + pChar + "'.");
            LOGGER.error("TODO [NHOJ-LANG-000001]", e);
            throw e;
        }

        LOGGER.debug("EXIT[valueOf=char;Trinary=" + trinary + "]");

        return trinary;
    }

    /**
     * the trinary code character.
     */
    private final char trinaryChar;

    /**
     * Constructor Trinary enum with its representations.
     *
     * @param pTrinaryChar
     *            the Trinary enum's representation as a char.
     */
    Trinary(final char pTrinaryChar) {

        final Logger LOGGER = LoggerFactory.getLogger(Trinary.class);

        LOGGER.debug("ENTRY[TrinaryChar=" + pTrinaryChar + "]");

        this.trinaryChar = pTrinaryChar;
    }

    /**
     * Get the Trinary enum as a char.
     *
     * @return the Trinary enum represented as a char.
     */
    public char getAsChar() {

        return this.trinaryChar;
    }

}
