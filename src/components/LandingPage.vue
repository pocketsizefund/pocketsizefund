<template>
    <body>
        <div class='container'>
            <div class='top'>
                <div class='logo'>
                    <span>
                        psf
                    </span>
                </div>
                <div class='link-twitter'>
                    <a 
                        href='https://twitter.com/pocketsizefund' 
                        target='_blank'
                    >
                        <svg 
                            xmlns="http://www.w3.org/2000/svg" 
                            height="1em" 
                            viewBox="0 0 512 512"><!--! Font Awesome Free 6.4.0 by @fontawesome - https://fontawesome.com License - https://fontawesome.com/license (Commercial License) Copyright 2023 Fonticons, Inc. --><path d="M459.37 151.716c.325 4.548.325 9.097.325 13.645 0 138.72-105.583 298.558-298.558 298.558-59.452 0-114.68-17.219-161.137-47.106 8.447.974 16.568 1.299 25.34 1.299 49.055 0 94.213-16.568 130.274-44.832-46.132-.975-84.792-31.188-98.112-72.772 6.498.974 12.995 1.624 19.818 1.624 9.421 0 18.843-1.3 27.614-3.573-48.081-9.747-84.143-51.98-84.143-102.985v-1.299c13.969 7.797 30.214 12.67 47.431 13.319-28.264-18.843-46.781-51.005-46.781-87.391 0-19.492 5.197-37.36 14.294-52.954 51.655 63.675 129.3 105.258 216.365 109.807-1.624-7.797-2.599-15.918-2.599-24.04 0-57.828 46.782-104.934 104.934-104.934 30.213 0 57.502 12.67 76.67 33.137 23.715-4.548 46.456-13.32 66.599-25.34-7.798 24.366-24.366 44.833-46.132 57.827 21.117-2.273 41.584-8.122 60.426-16.243-14.292 20.791-32.161 39.308-52.628 54.253z"/>
                        </svg>
                    </a>
                </div>
            </div>
            <div class='middle'>
                <h1 class='introduction'>
                    Recreational quantitative trading 🍊
                </h1>
                <!-- <div class='performance'>
                    <h2>Performance</h2>
                    <br/>
                    <table v-if='errorMessage === ""'>
                    </table>
                    <span v-if='errorMessage !== ""'>
                        {{ errorMessage }}
                    </span>
                </div> -->
            </div>
        </div>
    </body>
</template>

<script>
import axios from 'axios';

export default {
    name: 'LandingPage',
    data: function() {
        return {
            errorMessage: '',
        };
    },
    created: function () {
        const url = process.env.VUE_APP_GET_PERFORMANCE_API_ENDPOINT;

        const config = {
            headers: {
                'x-pocket-size-fund-api-key': process.env.VUE_APP_API_KEY,
            }
        }

        axios.get(url, config)
            .then((/*response*/) => {
                // TODO: implement after backend is complete
            })
            .catch(() => {
                this.$data.errorMessage = 'Data is not available yet.';
            }
        );
    },
};
</script>

<style>
body {
    font-family: 'Lexend', sans-serif;
    background-image: url('../assets/background-oranges.jpg');
    background-repeat: no-repeat;
    background-size: cover;
    background-attachment: fixed;
    height: 100%;
    width: 100%;
    color: #ffffff;
}

.container {
    height: 100%;
    margin: auto;
    width: 34rem;
    padding: 2rem 1rem 0rem;

}

.top > div {
    display: inline-block;
}

.logo {
    font-size: 3rem;
    font-weight: 900;
}

.link-twitter {
    float: right;
    font-size: 3rem;
}

a > svg {
    fill: #ffffff;
    font-size: 2rem;
}

.middle {
    padding-top: 6rem;
}

.introduction {
    font-size: 4rem;
    font-weight: 900;
    align-content: center;
}

.performance {
    padding-top: 2rem;
    font-size: 1.5rem;
}

@media screen and (max-width: 580px) {
    .container {
        width: 100%;
    }

    .middle {
        padding-top: 2rem;
    }

    .introduction {
        font-size: 2.5rem;
    }
}
</style>