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

import org.testng.annotations.Test;

import java.util.Set;
import java.util.TreeSet;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsEqual.equalTo;
import static org.hamcrest.core.IsNot.not;
import static org.hamcrest.core.IsNull.notNullValue;

/**
 * Test suite for <code>Trinary</code> enum.
 * 
 * @author nhojpatrick
 */
public class TrinaryTest {

    @Test
    public void trinary_equals_FALSE_FALSE() {

        final Trinary trinary = Trinary.FALSE;

        assertThat("Trinary should not be null", trinary, is(notNullValue()));
        assertThat("Unexpected Trunary.equals(Object)", trinary, is(equalTo(Trinary.FALSE)));
    }

    @Test
    public void trinary_equals_FALSE_TRUE() {

        final Trinary trinary = Trinary.FALSE;

        assertThat("Trinary should not be null", trinary, is(notNullValue()));
        assertThat("Unexpected Trunary.equals(Object)", trinary, is(not(equalTo(Trinary.TRUE))));
    }

    @Test
    public void trinary_equals_FALSE_UNKNOWN() {

        final Trinary trinary = Trinary.FALSE;

        assertThat("Trinary should not be null", trinary, is(notNullValue()));
        assertThat("Unexpected Trunary.equals(Object)", trinary, is(not(equalTo(Trinary.UNKNOWN))));
    }

    @Test
    public void trinary_equals_TRUE_FALSE() {

        final Trinary trinary = Trinary.TRUE;

        assertThat("Trinary should not be null", trinary, is(notNullValue()));
        assertThat("Unexpected Trunary.equals(Object)", trinary, is(not(equalTo(Trinary.FALSE))));
    }

    @Test
    public void trinary_equals_TRUE_TRUE() {

        final Trinary trinary = Trinary.TRUE;

        assertThat("Trinary should not be null", trinary, is(notNullValue()));
        assertThat("Unexpected Trunary.equals(Object)", trinary, is(equalTo(Trinary.TRUE)));
    }

    @Test
    public void trinary_equals_TRUE_UNKNOWN() {

        final Trinary trinary = Trinary.TRUE;

        assertThat("Trinary should not be null", trinary, is(notNullValue()));
        assertThat("Unexpected Trunary.equals(Object)", trinary, is(not(equalTo(Trinary.UNKNOWN))));
    }

    @Test
    public void trinary_equals_UNKNOWN_FALSE() {

        final Trinary trinary = Trinary.UNKNOWN;

        assertThat("Trinary should not be null", trinary, is(notNullValue()));
        assertThat("Unexpected Trunary.equals(Object)", trinary, is(not(equalTo(Trinary.FALSE))));
    }

    @Test
    public void trinary_equals_UNKNOWN_TRUE() {

        final Trinary trinary = Trinary.UNKNOWN;

        assertThat("Trinary should not be null", trinary, is(notNullValue()));
        assertThat("Unexpected Trunary.equals(Object)", trinary, is(not(equalTo(Trinary.TRUE))));
    }

    @Test
    public void trinary_equals_UNKNOWN_UNKNOWN() {

        final Trinary trinary = Trinary.UNKNOWN;

        assertThat("Trinary should not be null", trinary, is(notNullValue()));
        assertThat("Unexpected Trunary.equals(Object)", trinary, is(equalTo(Trinary.UNKNOWN)));
    }

    @Test
    public void trinary_getAsChar_FALSE() {

        final Trinary trinary = Trinary.FALSE;

        assertThat("Trinary should not be null", trinary, is(notNullValue()));
        assertThat("Unexpected Trunary.getAsChar()", trinary.getAsChar(), is(equalTo('F')));
    }

    @Test
    public void trinary_getAsChar_TRUE() {

        final Trinary trinary = Trinary.TRUE;

        assertThat("Trinary should not be null", trinary, is(notNullValue()));
        assertThat("Unexpected Trunary.getAsChar()", trinary.getAsChar(), is(equalTo('T')));
    }

    @Test
    public void trinary_getAsChar_UNKNOWN() {

        final Trinary trinary = Trinary.UNKNOWN;

        assertThat("Trinary should not be null", trinary, is(notNullValue()));
        assertThat("Unexpected Trunary.getAsChar()", trinary.getAsChar(), is(equalTo('U')));
    }

    @Test
    public void trinary_hashCode_FALSE_FALSE() {

        final Trinary trinary = Trinary.FALSE;

        assertThat("Trinary should not be null", trinary, is(notNullValue()));
        assertThat("Unexpected Trunary.hashCode()", trinary.hashCode(), is(equalTo(Trinary.FALSE.hashCode())));
    }

    @Test
    public void trinary_hashCode_FALSE_TRUE() {

        final Trinary trinary = Trinary.FALSE;

        assertThat("Trinary should not be null", trinary, is(notNullValue()));
        assertThat("Unexpected Trunary.hashCode()", trinary.hashCode(), is(not(equalTo(Trinary.TRUE.hashCode()))));
    }

    @Test
    public void trinary_hashCode_FALSE_UNKNOWN() {

        final Trinary trinary = Trinary.FALSE;

        assertThat("Trinary should not be null", trinary, is(notNullValue()));
        assertThat("Unexpected Trunary.hashCode()", trinary.hashCode(), is(not(equalTo(Trinary.UNKNOWN.hashCode()))));
    }

    @Test
    public void trinary_hashCode_TRUE_FALSE() {

        final Trinary trinary = Trinary.TRUE;

        assertThat("Trinary should not be null", trinary, is(notNullValue()));
        assertThat("Unexpected Trunary.hashCode()", trinary.hashCode(), is(not(equalTo(Trinary.FALSE.hashCode()))));
    }

    @Test
    public void trinary_hashCode_TRUE_TRUE() {

        final Trinary trinary = Trinary.TRUE;

        assertThat("Trinary should not be null", trinary, is(notNullValue()));
        assertThat("Unexpected Trunary.hashCode()", trinary.hashCode(), is(equalTo(Trinary.TRUE.hashCode())));
    }

    @Test
    public void trinary_hashCode_TRUE_UNKNOWN() {

        final Trinary trinary = Trinary.TRUE;

        assertThat("Trinary should not be null", trinary, is(notNullValue()));
        assertThat("Unexpected Trunary.hashCode()", trinary, is(not(equalTo(Trinary.UNKNOWN.hashCode()))));
    }

    @Test
    public void trinary_hashCode_UNKNOWN_FALSE() {

        final Trinary trinary = Trinary.UNKNOWN;

        assertThat("Trinary should not be null", trinary, is(notNullValue()));
        assertThat("Unexpected Trunary.hashCode()", trinary, is(not(equalTo(Trinary.FALSE.hashCode()))));
    }

    @Test
    public void trinary_hashCode_UNKNOWN_TRUE() {

        final Trinary trinary = Trinary.UNKNOWN;

        assertThat("Trinary should not be null", trinary, is(notNullValue()));
        assertThat("Unexpected Trunary.hashCode()", trinary, is(not(equalTo(Trinary.TRUE.hashCode()))));
    }

    @Test
    public void trinary_hashCode_UNKNOWN_UNKNOWN() {

        final Trinary trinary = Trinary.UNKNOWN;

        assertThat("Trinary should not be null", trinary, is(notNullValue()));
        assertThat("Unexpected Trunary.hashCode()", trinary.hashCode(), is(equalTo(Trinary.UNKNOWN.hashCode())));
    }

    @Test
    public void trinary_toString_FALSE() {

        final Trinary trinary = Trinary.FALSE;

        assertThat("Trinary should not be null", trinary, is(notNullValue()));
        assertThat("Unexpected Trunary.toString()", trinary.toString(), is(equalTo("FALSE")));
    }

    @Test
    public void trinary_toString_TRUE() {

        final Trinary trinary = Trinary.TRUE;

        assertThat("Trinary should not be null", trinary, is(notNullValue()));
        assertThat("Unexpected Trunary.toString()", trinary.toString(), is(equalTo("TRUE")));
    }

    @Test
    public void trinary_toString_UNKNOWN() {

        final Trinary trinary = Trinary.UNKNOWN;

        assertThat("Trinary should not be null", trinary, is(notNullValue()));
        assertThat("Unexpected Trunary.toString()", trinary.toString(), is(equalTo("UNKNOWN")));
    }

    @Test
    public void trinary_valueOf_Boolean_BooleanFALSE() {

        final Trinary trinary = Trinary.valueOf(Boolean.FALSE);

        assertThat("Trinary.valueOf(Boolean) should not have returned null", trinary, is(notNullValue()));
        assertThat("Unexpected Trunary.name()", trinary.name(), is(equalTo("FALSE")));
    }

    @Test
    public void trinary_valueOf_Boolean_BooleanTRUE() {

        final Trinary trinary = Trinary.valueOf(Boolean.TRUE);

        assertThat("Trinary.valueOf(Boolean) should not have returned null", trinary, is(notNullValue()));
        assertThat("Unexpected Trunary.name()", trinary.name(), is(equalTo("TRUE")));
    }

    @Test
    public void trinary_valueOf_Boolean_false() {

        final Trinary trinary = Trinary.valueOf(false);

        assertThat("Trinary.valueOf(Boolean) should not have returned null", trinary, is(notNullValue()));
        assertThat("Unexpected Trunary.name()", trinary.name(), is(equalTo("FALSE")));
    }

    @Test
    public void trinary_valueOf_Boolean_null() {

        final Trinary trinary = Trinary.valueOf((Boolean) null);

        assertThat("Trinary.valueOf(Boolean) should not have returned null", trinary, is(notNullValue()));
        assertThat("Unexpected Trunary.name()", trinary.name(), is(equalTo("UNKNOWN")));
    }

    @Test
    public void trinary_valueOf_Boolean_true() {

        final Trinary trinary = Trinary.valueOf(true);

        assertThat("Trinary.valueOf(Boolean) should not have returned null", trinary, is(notNullValue()));
        assertThat("Unexpected Trunary.name()", trinary.name(), is(equalTo("TRUE")));
    }

    @Test
    public void trinary_valueOf_char_F() {

        final Trinary trinary = Trinary.valueOf('F');

        assertThat("Trinary.valueOf(char) should not have returned null", trinary, is(notNullValue()));
        assertThat("Unexpected Trunary.name()", trinary.name(), is(equalTo("FALSE")));
    }

    @Test(expectedExceptions = IllegalArgumentException.class)
    public void trinary_valueOf_char_invalidDotChar() {

        Trinary.valueOf('.');
    }

    @Test(expectedExceptions = IllegalArgumentException.class)
    public void trinary_valueOf_char_invalidSpaceChar() {

        Trinary.valueOf(' ');
    }

    @Test
    public void trinary_valueOf_char_T() {

        final Trinary trinary = Trinary.valueOf('T');

        assertThat("Trinary.valueOf(char) should not have returned null", trinary, is(notNullValue()));
        assertThat("Unexpected Trunary.name()", trinary.name(), is(equalTo("TRUE")));
    }

    @Test
    public void trinary_valueOf_char_U() {

        final Trinary trinary = Trinary.valueOf('U');

        assertThat("Trinary.valueOf(char) should not have returned null", trinary, is(notNullValue()));
        assertThat("Unexpected Trunary.name()", trinary.name(), is(equalTo("UNKNOWN")));
    }

    @Test(enabled = false)
    public void trinary_valueOf_String_false() {

        final Trinary trinary = Trinary.valueOf("false");

        assertThat("Trinary.valueOf(String) should not have returned null", trinary, is(notNullValue()));
        assertThat("Unexpected Trunary.name()", trinary.name(), is(equalTo("FALSE")));
    }

    @Test
    public void trinary_valueOf_String_FALSE() {

        final Trinary trinary = Trinary.valueOf("FALSE");

        assertThat("Trinary.valueOf(String) should not have returned null", trinary, is(notNullValue()));
        assertThat("Unexpected Trunary.name()", trinary.name(), is(equalTo("FALSE")));
    }

    @Test(expectedExceptions = IllegalArgumentException.class)
    public void trinary_valueOf_String_invalidDotString() {

        Trinary.valueOf(".");
    }

    @Test(expectedExceptions = IllegalArgumentException.class)
    public void trinary_valueOf_String_invalidEmptyString() {

        Trinary.valueOf("");
    }

    @Test(expectedExceptions = IllegalArgumentException.class)
    public void trinary_valueOf_String_invalidSpaceString() {

        Trinary.valueOf(" ");
    }

    @Test(expectedExceptions = NullPointerException.class)
    public void trinary_valueOf_String_null() {

        Trinary.valueOf((String) null);
    }

    @Test(expectedExceptions = IllegalArgumentException.class)
    public void trinary_valueOf_String_plasibo() {

        Trinary.valueOf("plasibo");
    }

    @Test(enabled = false)
    public void trinary_valueOf_String_true() {

        final Trinary trinary = Trinary.valueOf("true");

        assertThat("Trinary.valueOf(String) should not have returned null", trinary, is(notNullValue()));
        assertThat("Unexpected Trunary.name()", trinary.name(), is(equalTo("true")));
    }

    @Test
    public void trinary_valueOf_String_TRUE() {

        final Trinary trinary = Trinary.valueOf("TRUE");

        assertThat("Trinary.valueOf(String) should not have returned null", trinary, is(notNullValue()));
        assertThat("Unexpected Trunary.name()", trinary.name(), is(equalTo("TRUE")));
    }

    @Test(enabled = false)
    public void trinary_valueOf_String_unknown() {

        final Trinary trinary = Trinary.valueOf("unknown");

        assertThat("Trinary.valueOf(String) should not have returned null", trinary, is(notNullValue()));
        assertThat("Unexpected Trunary.name()", trinary.name(), is(equalTo("unknown")));
    }

    @Test
    public void trinary_valueOf_String_UNKNOWN() {

        final Trinary trinary = Trinary.valueOf("UNKNOWN");

        assertThat("Trinary.valueOf(String) should not have returned null", trinary, is(notNullValue()));
        assertThat("Unexpected Trunary.name()", trinary.name(), is(equalTo("UNKNOWN")));
    }

    @Test
    public void trinary_values_checkingName() {

        final Trinary[] trinaries = Trinary.values();

        assertThat("Trinary.values() should not return null", trinaries, is(notNullValue()));

        final Set<String> entries = new TreeSet<>();
        for (final Trinary trinary : trinaries) {
            final String trinaryName = trinary.name();
            entries.add(trinaryName);
        }
        assertThat("Unexpected Trinary's", entries.toString(), is(equalTo("[FALSE, TRUE, UNKNOWN]")));
    }

    @Test
    public void trinary_values_checkingToString() {

        final Trinary[] trinaries = Trinary.values();

        assertThat("Trinary.values() should not return null", trinaries, is(notNullValue()));

        final Set<String> entries = new TreeSet<>();
        for (final Trinary trinary : trinaries) {
            final String trinaryName = trinary.toString();
            entries.add(trinaryName);
        }
        assertThat("Unexpected Trinary's", entries.toString(), is(equalTo("[FALSE, TRUE, UNKNOWN]")));
    }

    @Test
    public void trinary_values_size() {

        final Trinary[] trinaries = Trinary.values();

        assertThat("Trinary.values() should not return null", trinaries, is(notNullValue()));
        assertThat("Unexpected number of Trinary's", trinaries.length, is(equalTo(3)));
    }

}
