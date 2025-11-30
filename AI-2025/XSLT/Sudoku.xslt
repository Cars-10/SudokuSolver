<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
    <xsl:output method="text"/>

    <xsl:template match="/">
        <xsl:variable name="result-str">
            <xsl:call-template name="solve">
                <xsl:with-param name="board" select="normalize-space(puzzle)"/>
                <xsl:with-param name="iters" select="0"/>
            </xsl:call-template>
        </xsl:variable>

        <xsl:variable name="solution" select="substring-after($result-str, '|')"/>
        <xsl:variable name="final-iters" select="substring-before($result-str, '|')"/>
        
        <xsl:choose>
            <xsl:when test="string-length($solution) > 0">
                <xsl:text>Puzzle:&#10;</xsl:text>
                <xsl:call-template name="print-board">
                    <xsl:with-param name="board" select="normalize-space(puzzle)"/>
                </xsl:call-template>
                <xsl:text>&#10;Solved in Iterations=</xsl:text>
                <xsl:value-of select="$final-iters"/>
                <xsl:text>&#10;</xsl:text>
                <xsl:call-template name="print-board">
                    <xsl:with-param name="board" select="$solution"/>
                </xsl:call-template>
            </xsl:when>
            <xsl:otherwise>
                <xsl:text>No solution found&#10;</xsl:text>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template name="solve">
        <xsl:param name="board"/>
        <xsl:param name="index" select="1"/>
        <xsl:param name="iters"/>
        
        <xsl:choose>
            <xsl:when test="$index > 81">
                <xsl:value-of select="concat($iters, '|', $board)"/>
            </xsl:when>
            <xsl:otherwise>
                <!-- Calculate position in string: (index-1)*2 + 1 -->
                <xsl:variable name="pos" select="($index - 1) * 2 + 1"/>
                <xsl:variable name="cell" select="substring($board, $pos, 1)"/>
                
                <xsl:choose>
                    <xsl:when test="$cell != '0'">
                        <xsl:call-template name="solve">
                            <xsl:with-param name="board" select="$board"/>
                            <xsl:with-param name="index" select="$index + 1"/>
                            <xsl:with-param name="iters" select="$iters"/>
                        </xsl:call-template>
                    </xsl:when>
                    <xsl:otherwise>
                        <xsl:variable name="row" select="floor(($index - 1) div 9)"/>
                        <xsl:variable name="col" select="($index - 1) mod 9"/>
                        
                        <xsl:call-template name="try-values">
                            <xsl:with-param name="board" select="$board"/>
                            <xsl:with-param name="index" select="$index"/>
                            <xsl:with-param name="val" select="1"/>
                            <xsl:with-param name="row" select="$row"/>
                            <xsl:with-param name="col" select="$col"/>
                            <xsl:with-param name="iters" select="$iters"/>
                        </xsl:call-template>
                    </xsl:otherwise>
                </xsl:choose>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template name="try-values">
        <xsl:param name="board"/>
        <xsl:param name="index"/>
        <xsl:param name="val"/>
        <xsl:param name="row"/>
        <xsl:param name="col"/>
        <xsl:param name="iters"/>
        
        <xsl:choose>
            <xsl:when test="$val > 9">
                <xsl:value-of select="concat($iters, '|')"/>
            </xsl:when>
            <xsl:otherwise>
                <xsl:variable name="current-iters" select="$iters + 1"/>
                
                <xsl:variable name="possible">
                    <!-- Check Row -->
                    <xsl:variable name="row-start" select="$row * 18 + 1"/>
                    <xsl:variable name="row-str" select="substring($board, $row-start, 18)"/>
                    
                    <xsl:choose>
                        <xsl:when test="contains($row-str, $val)">false</xsl:when>
                        <xsl:otherwise>
                            <!-- Check Col -->
                            <xsl:variable name="c0" select="$col * 2 + 1"/>
                            <xsl:choose>
                                <xsl:when test="
                                    substring($board, $c0, 1) = $val or
                                    substring($board, $c0 + 18, 1) = $val or
                                    substring($board, $c0 + 36, 1) = $val or
                                    substring($board, $c0 + 54, 1) = $val or
                                    substring($board, $c0 + 72, 1) = $val or
                                    substring($board, $c0 + 90, 1) = $val or
                                    substring($board, $c0 + 108, 1) = $val or
                                    substring($board, $c0 + 126, 1) = $val or
                                    substring($board, $c0 + 144, 1) = $val
                                ">false</xsl:when>
                                <xsl:otherwise>
                                    <!-- Check Box -->
                                    <xsl:variable name="box-r" select="floor($row div 3) * 3"/>
                                    <xsl:variable name="box-c" select="floor($col div 3) * 3"/>
                                    <xsl:variable name="b0" select="($box-r * 9 + $box-c) * 2 + 1"/>
                                    <xsl:choose>
                                        <xsl:when test="
                                            substring($board, $b0, 1) = $val or
                                            substring($board, $b0 + 2, 1) = $val or
                                            substring($board, $b0 + 4, 1) = $val or
                                            substring($board, $b0 + 18, 1) = $val or
                                            substring($board, $b0 + 20, 1) = $val or
                                            substring($board, $b0 + 22, 1) = $val or
                                            substring($board, $b0 + 36, 1) = $val or
                                            substring($board, $b0 + 38, 1) = $val or
                                            substring($board, $b0 + 40, 1) = $val
                                        ">false</xsl:when>
                                        <xsl:otherwise>true</xsl:otherwise>
                                    </xsl:choose>
                                </xsl:otherwise>
                            </xsl:choose>
                        </xsl:otherwise>
                    </xsl:choose>
                </xsl:variable>
                
                <xsl:choose>
                    <xsl:when test="$possible = 'true'">
                        <xsl:variable name="pos" select="($index - 1) * 2 + 1"/>
                        <xsl:variable name="new-board" select="concat(substring($board, 1, $pos - 1), $val, substring($board, $pos + 1))"/>
                        <xsl:variable name="res-str">
                            <xsl:call-template name="solve">
                                <xsl:with-param name="board" select="$new-board"/>
                                <xsl:with-param name="index" select="$index + 1"/>
                                <xsl:with-param name="iters" select="$current-iters"/>
                            </xsl:call-template>
                        </xsl:variable>
                        
                        <xsl:variable name="sol" select="substring-after($res-str, '|')"/>
                        <xsl:variable name="ret-iters" select="substring-before($res-str, '|')"/>
                        
                        <xsl:choose>
                            <xsl:when test="string-length($sol) > 0">
                                <xsl:value-of select="$res-str"/>
                            </xsl:when>
                            <xsl:otherwise>
                                <xsl:call-template name="try-values">
                                    <xsl:with-param name="board" select="$board"/>
                                    <xsl:with-param name="index" select="$index"/>
                                    <xsl:with-param name="val" select="$val + 1"/>
                                    <xsl:with-param name="row" select="$row"/>
                                    <xsl:with-param name="col" select="$col"/>
                                    <xsl:with-param name="iters" select="$ret-iters"/>
                                </xsl:call-template>
                            </xsl:otherwise>
                        </xsl:choose>
                    </xsl:when>
                    <xsl:otherwise>
                        <xsl:call-template name="try-values">
                            <xsl:with-param name="board" select="$board"/>
                            <xsl:with-param name="index" select="$index"/>
                            <xsl:with-param name="val" select="$val + 1"/>
                            <xsl:with-param name="row" select="$row"/>
                            <xsl:with-param name="col" select="$col"/>
                            <xsl:with-param name="iters" select="$current-iters"/>
                        </xsl:call-template>
                    </xsl:otherwise>
                </xsl:choose>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template name="print-board">
        <xsl:param name="board"/>
        <xsl:if test="string-length($board) > 0">
            <xsl:value-of select="substring($board, 1, 18)"/>
            <xsl:text>&#10;</xsl:text>
            <xsl:call-template name="print-board">
                <xsl:with-param name="board" select="substring($board, 19)"/>
            </xsl:call-template>
        </xsl:if>
    </xsl:template>

</xsl:stylesheet>
